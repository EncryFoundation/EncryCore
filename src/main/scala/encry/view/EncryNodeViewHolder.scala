package encry.view

import java.io.File
import akka.actor.{Actor, Props}
import akka.persistence.RecoveryCompleted
import akka.pattern._
import encry.EncryApp._
import encry.consensus.History.ProgressInfo
import encry.local.explorer.BlockListener.ChainSwitching
import encry.local.TransactionGenerator.{amountD, FetchWalletData, GenerateTransaction, WalletData}
import encry.modifiers._
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.{EncryBlockHeader, EncryBlockHeaderSerializer}
import encry.modifiers.history.block.payload.{EncryBlockPayload, EncryBlockPayloadSerializer}
import encry.modifiers.history.{ADProofs, ADProofSerializer}
import encry.modifiers.mempool.{EncryTransactionSerializer, Transaction}
import encry.modifiers.state.box.{AssetBox, EncryProposition}
import encry.network.DeliveryManager.{ContinueSync, FullBlockChainSynced, StopSync}
import encry.network.EncryNodeViewSynchronizer.ReceivableMessages._
import encry.network.ModifiersHolder.{RequestedModifiers, SendBlocks}
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.stats.StatsSender._
import encry.view.EncryNodeViewHolder.ReceivableMessages._
import encry.view.EncryNodeViewHolder.{DownloadRequest, _}
import encry.view.history.EncryHistory
import encry.view.mempool.EncryMempool
import encry.view.state._
import encry.view.wallet.EncryWallet
import encry.{EncryApp, ModifierId, ModifierTypeId, VersionTag}
import encry.utils.Logging
import org.apache.commons.io.FileUtils
import org.encryfoundation.common.Algos
import org.encryfoundation.common.serialization.Serializer
import org.encryfoundation.common.transaction.Proposition
import org.encryfoundation.common.utils.TaggedTypes.ADDigest
import scala.annotation.tailrec
import scala.concurrent.Future
import scala.collection.{mutable, IndexedSeq, Seq}
import scala.util.{Failure, Success, Try}

class EncryNodeViewHolder[StateType <: EncryState[StateType]] extends Actor with Logging {

  case class NodeView(history: EncryHistory, state: StateType, wallet: EncryWallet, mempool: EncryMempool)

  var applicationsSuccessful: Boolean = true
  var nodeView: NodeView = restoreState().getOrElse(genesisState)
  val modifiersCache: EncryModifiersCache = EncryModifiersCache(1000)
  val modifierSerializers: Map[ModifierTypeId, Serializer[_ <: NodeViewModifier]] = Map(
    EncryBlockHeader.modifierTypeId -> EncryBlockHeaderSerializer,
    EncryBlockPayload.modifierTypeId -> EncryBlockPayloadSerializer,
    ADProofs.modifierTypeId -> ADProofSerializer,
    Transaction.ModifierTypeId -> EncryTransactionSerializer
  )

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    reason.printStackTrace()
    System.exit(100)
  }

  override def postStop(): Unit = {
    warn(s"Stopping EncryNodeViewHolder")
    nodeView.history.closeStorage()
    nodeView.state.closeStorage()
  }

  override def receive: Receive = {
    case BlocksFromLocalPersistence(blocks) if settings.levelDb.recoverMode =>
      blocks.foreach { block =>
        pmodModifyRecovery(block) match {
          case Success(_) =>
            info(s"Block ${block.encodedId} from recovery applied successfully")
          case Failure(th) =>
            warn(s"Failed to apply block ${block.encodedId} from recovery caused $th")
            applicationsSuccessful = false
            peerManager ! RecoveryCompleted
        }
      }
      if (applicationsSuccessful) sender ! SendBlocks
    case ModifiersFromRemote(modifierTypeId, remoteObjects) =>
      if (modifiersCache.isEmpty && nodeView.history.isHeadersChainSynced) nodeViewSynchronizer ! StopSync
      modifierSerializers.get(modifierTypeId).foreach { companion =>
        remoteObjects.flatMap(r => companion.parseBytes(r).toOption).foreach {
          case tx: Transaction@unchecked if tx.modifierTypeId == Transaction.ModifierTypeId => txModify(tx)
          case pmod: EncryPersistentModifier@unchecked =>
            if (nodeView.history.contains(pmod.id) || modifiersCache.contains(key(pmod.id)))
              warn(s"Received modifier ${pmod.encodedId} that is already in history")
            else {
              modifiersCache.put(key(pmod.id), pmod)
              if (settings.levelDb.enable)
                context.actorSelection("/user/modifiersHolder") ! RequestedModifiers(modifierTypeId, Seq(pmod))
            }
        }
        info(s"Cache before(${modifiersCache.size})")
        computeApplications()
        if (modifiersCache.isEmpty || !nodeView.history.isHeadersChainSynced) nodeViewSynchronizer ! ContinueSync
        info(s"Cache after(${modifiersCache.size})")
      }
    case lt: LocallyGeneratedTransaction[EncryProposition, Transaction] => txModify(lt.tx)
    case lm: LocallyGeneratedModifier[EncryPersistentModifier] =>
      info(s"Got locally generated modifier ${lm.pmod.encodedId} of type ${lm.pmod.modifierTypeId}")
      pmodModify(lm.pmod)
      if (settings.levelDb.enable) context.actorSelection("/user/modifiersHolder") ! lm
    case GetDataFromCurrentView(f) =>
      val result = f(CurrentView(nodeView.history, nodeView.state, nodeView.wallet, nodeView.mempool))
      result match {
        case resultFuture: Future[_] => resultFuture.pipeTo(sender())
        case _ => sender() ! result
      }
    case GetNodeViewChanges(history, state, vault, mempool) =>
      if (history) sender() ! ChangedHistory(nodeView.history)
      if (state) sender() ! ChangedState(nodeView.state)
      if (mempool) sender() ! ChangedMempool(nodeView.mempool)
    case CompareViews(peer, modifierTypeId, modifierIds) =>
      val ids: Seq[ModifierId] = modifierTypeId match {
        case typeId: ModifierTypeId if typeId == Transaction.ModifierTypeId => nodeView.mempool.notIn(modifierIds)
        case _ => modifierIds.filterNot(mid => nodeView.history.contains(mid) || modifiersCache.contains(key(mid)))
      }
      sender() ! RequestFromLocal(peer, modifierTypeId, ids)
    case FetchWalletData(limit: Int, minimalFeeD: Int) =>
      val wallet: EncryWallet = nodeView.wallet
      val availableBoxes: Seq[AssetBox] = wallet.walletStorage.allBoxes.filter(_.isAmountCarrying).map(_.asInstanceOf[AssetBox])
      if (availableBoxes.map(_.amount).sum >= limit * (amountD + minimalFeeD))
        sender() ! GenerateTransaction(WalletData(wallet.accountManager.mandatoryAccount, availableBoxes))
    case a: Any =>
      error(s"Strange input: $a")
  }

  def computeApplications(): Unit =
    modifiersCache.popCandidate(nodeView.history) match {
      case Some(mod) =>
        pmodModify(mod)
        computeApplications()
      case None => Unit
    }

  def key(id: ModifierId): mutable.WrappedArray.ofByte = new mutable.WrappedArray.ofByte(id)

  def updateNodeView(updatedHistory: Option[EncryHistory] = None,
                     updatedState: Option[StateType] = None,
                     updatedVault: Option[EncryWallet] = None,
                     updatedMempool: Option[EncryMempool] = None): Unit = {
    val newNodeView: NodeView = NodeView(updatedHistory.getOrElse(nodeView.history),
      updatedState.getOrElse(nodeView.state),
      updatedVault.getOrElse(nodeView.wallet),
      updatedMempool.getOrElse(nodeView.mempool))
    if (updatedHistory.nonEmpty) context.system.eventStream.publish(ChangedHistory(newNodeView.history))
    if (updatedState.nonEmpty) context.system.eventStream.publish(ChangedState(newNodeView.state))
    if (updatedMempool.nonEmpty) context.system.eventStream.publish(ChangedMempool(newNodeView.mempool))
    nodeView = newNodeView
  }

  def extractTransactions(mod: EncryPersistentModifier): Seq[Transaction] = mod match {
    case tcm: TransactionsCarryingPersistentNodeViewModifier[EncryProposition, Transaction] => tcm.transactions
    case _ => Seq()
  }

  def updateMemPool(blocksRemoved: Seq[EncryPersistentModifier], blocksApplied: Seq[EncryPersistentModifier],
                    memPool: EncryMempool, state: StateType): EncryMempool = {
    val rolledBackTxs: Seq[Transaction] = blocksRemoved.flatMap(extractTransactions)
    val appliedTxs: Seq[Transaction] = blocksApplied.flatMap(extractTransactions)
    memPool.putWithoutCheck(rolledBackTxs).filter { tx =>
      !appliedTxs.exists(t => t.id sameElements tx.id) && {
        state match {
          case v: TransactionValidation[EncryProposition, Transaction] => v.validate(tx).isSuccess
          case _ => true
        }
      }
    }
  }

  def requestDownloads(pi: ProgressInfo[EncryPersistentModifier]): Unit =
    pi.toDownload.foreach { case (tid, id) => nodeViewSynchronizer ! DownloadRequest(tid, id) }

  def trimChainSuffix(suffix: IndexedSeq[EncryPersistentModifier], rollbackPoint: ModifierId):
  IndexedSeq[EncryPersistentModifier] = {
    val idx: Int = suffix.indexWhere(_.id.sameElements(rollbackPoint))
    if (idx == -1) IndexedSeq() else suffix.drop(idx)
  }

  @tailrec
  private def updateState(history: EncryHistory,
                          state: StateType,
                          progressInfo: ProgressInfo[EncryPersistentModifier],
                          suffixApplied: IndexedSeq[EncryPersistentModifier]):
  (EncryHistory, Try[StateType], Seq[EncryPersistentModifier]) = {
    case class UpdateInformation(history: EncryHistory,
                                 state: StateType,
                                 failedMod: Option[EncryPersistentModifier],
                                 alternativeProgressInfo: Option[ProgressInfo[EncryPersistentModifier]],
                                 suffix: IndexedSeq[EncryPersistentModifier])

    requestDownloads(progressInfo)
    val branchingPointOpt: Option[VersionTag] = progressInfo.branchPoint.map(VersionTag !@@ _)
    val (stateToApplyTry: Try[StateType], suffixTrimmed: IndexedSeq[EncryPersistentModifier]) =
      if (progressInfo.chainSwitchingNeeded) {
        branchingPointOpt.map { branchPoint =>
          if (!state.version.sameElements(branchPoint))
            state.rollbackTo(branchPoint) -> trimChainSuffix(suffixApplied, ModifierId !@@ branchPoint)
          else Success(state) -> IndexedSeq()
        }.getOrElse(Failure(new Exception("Trying to rollback when branchPoint is empty")))
      } else Success(state) -> suffixApplied

    stateToApplyTry match {
      case Success(stateToApply) =>
        context.system.eventStream.publish(RollbackSucceed(branchingPointOpt))
        val u0: UpdateInformation = UpdateInformation(history, stateToApply, None, None, suffixTrimmed)
        val uf: UpdateInformation = progressInfo.toApply.foldLeft(u0) { case (u, modToApply) =>
          if (u.failedMod.isEmpty) u.state.applyModifier(modToApply) match {
            case Success(stateAfterApply) =>
              val newHis: EncryHistory = history.reportModifierIsValid(modToApply)
              context.system.eventStream.publish(SemanticallySuccessfulModifier(modToApply))
              UpdateInformation(newHis, stateAfterApply, None, None, u.suffix :+ modToApply)
            case Failure(e) =>
              val (newHis: EncryHistory, newProgressInfo: ProgressInfo[EncryPersistentModifier]) =
                history.reportModifierIsInvalid(modToApply, progressInfo)
              nodeViewSynchronizer ! SemanticallyFailedModification(modToApply, e)
              UpdateInformation(newHis, u.state, Some(modToApply), Some(newProgressInfo), u.suffix)
          } else u
        }
        uf.failedMod match {
          case Some(_) => updateState(uf.history, uf.state, uf.alternativeProgressInfo.get, uf.suffix)
          case None => (uf.history, Success(uf.state), uf.suffix)
        }
      case Failure(e) =>
        error(s"Rollback failed: $e")
        context.system.eventStream.publish(RollbackFailed(branchingPointOpt))
        EncryApp.forceStopApplication(500)
    }
  }

  def pmodModifyRecovery(block: EncryBlock): Try[Unit] = if (!nodeView.history.contains(block.id)) Try {
    info(s"Trying to apply block ${block.encodedId} from recovery")
    pmodModify(block.header)
    nodeView.history.blockDownloadProcessor.updateBestBlock(block.header)
    pmodModify(block.payload)
  } else Success(Unit)

  def pmodModify(pmod: EncryPersistentModifier): Unit = if (!nodeView.history.contains(pmod.id)) {
    info(s"Apply modifier ${pmod.encodedId} of type ${pmod.modifierTypeId} to nodeViewHolder")
    if (settings.node.sendStat) context.system
      .actorSelection("user/statsSender") ! StartApplyingModif(pmod.id, pmod.modifierTypeId, System.currentTimeMillis())
    nodeView.history.append(pmod) match {
      case Success((historyBeforeStUpdate, progressInfo)) =>
        if (settings.node.sendStat)
          context.system.actorSelection("user/statsSender") ! EndOfApplyingModif(pmod.id)
        info(s"Going to apply modifications to the state: $progressInfo")
        nodeViewSynchronizer ! SyntacticallySuccessfulModifier(pmod)
        if (progressInfo.toApply.nonEmpty) {
          val startPoint: Long = System.currentTimeMillis()
          val (newHistory: EncryHistory, newStateTry: Try[StateType], blocksApplied: Seq[EncryPersistentModifier]) =
            updateState(historyBeforeStUpdate, nodeView.state, progressInfo, IndexedSeq())
          if (settings.node.sendStat)
            context.actorSelection("user/statsSender") ! StateUpdating(System.currentTimeMillis() - startPoint)
          newStateTry match {
            case Success(newMinState) =>
              val newMemPool: EncryMempool = updateMemPool(progressInfo.toRemove, blocksApplied, nodeView.mempool, newMinState)
              val newVault: EncryWallet = if (progressInfo.chainSwitchingNeeded)
                nodeView.wallet.rollback(VersionTag !@@ progressInfo.branchPoint.get).get
              else nodeView.wallet
              blocksApplied.foreach(newVault.scanPersistent)
              info(s"Persistent modifier ${pmod.encodedId} applied successfully")
              if (progressInfo.chainSwitchingNeeded)
                context.actorSelection("/user/blockListener") ! ChainSwitching(progressInfo.toRemove.map(_.id))
              if (settings.node.sendStat)
                newHistory.bestHeaderOpt.foreach(header =>
                  context.actorSelection("/user/statsSender") ! BestHeaderInChain(header))
              if (newHistory.isFullChainSynced) Seq(nodeViewSynchronizer, miner).foreach(_ ! FullBlockChainSynced)
              updateNodeView(Some(newHistory), Some(newMinState), Some(newVault), Some(newMemPool))
            case Failure(e) =>
              warn(s"Can`t apply persistent modifier (id: ${pmod.encodedId}, contents: $pmod) to minimal state because of: $e")
              updateNodeView(updatedHistory = Some(newHistory))
              nodeViewSynchronizer ! SemanticallyFailedModification(pmod, e)
          }
        } else {
          requestDownloads(progressInfo)
          updateNodeView(updatedHistory = Some(historyBeforeStUpdate))
        }
      case Failure(e) =>
        warn(s"Can`t apply persistent modifier (id: ${pmod.encodedId}, contents: $pmod) to history caused $e")
        nodeViewSynchronizer ! SyntacticallyFailedModification(pmod, e)
    }
  } else warn(s"Trying to apply modifier ${pmod.encodedId} that's already in history")

  def txModify(tx: Transaction): Unit = nodeView.mempool.put(tx) match {
    case Success(newPool) =>
      val newVault: EncryWallet = nodeView.wallet.scanOffchain(tx)
      updateNodeView(updatedVault = Some(newVault), updatedMempool = Some(newPool))
      nodeViewSynchronizer ! SuccessfulTransaction[EncryProposition, Transaction](tx)
    case Failure(e) =>
  }

  def genesisState: NodeView = {
    val stateDir: File = EncryState.getStateDir(settings)
    stateDir.mkdir()
    assert(stateDir.listFiles().isEmpty, s"Genesis directory $stateDir should always be empty")
    val state: StateType = {
      if (settings.node.stateMode.isDigest) EncryState.generateGenesisDigestState(stateDir, settings.node)
      else EncryState.generateGenesisUtxoState(stateDir, Some(self))
    }.asInstanceOf[StateType]
    val history: EncryHistory = EncryHistory.readOrGenerate(settings, timeProvider)
    val wallet: EncryWallet = EncryWallet.readOrGenerate(settings)
    val memPool: EncryMempool = EncryMempool.empty(settings, timeProvider, system)
    NodeView(history, state, wallet, memPool)
  }

  def restoreState(): Option[NodeView] = if (!EncryHistory.getHistoryObjectsDir(settings).listFiles.isEmpty)
    try {
      val history: EncryHistory = EncryHistory.readOrGenerate(settings, timeProvider)
      val wallet: EncryWallet = EncryWallet.readOrGenerate(settings)
      val memPool: EncryMempool = EncryMempool.empty(settings, timeProvider, system)
      val state: StateType = restoreConsistentState(EncryState.readOrGenerate(settings, Some(self)).asInstanceOf[StateType], history)
      Some(NodeView(history, state, wallet, memPool))
    } catch {
      case ex: Throwable =>
        info(s"${ex.getMessage} during state restore. Recover from Modifiers holder!")
        new File(settings.directory).listFiles.foreach(dir => {
          FileUtils.cleanDirectory(dir)
        })
        Some(genesisState)
    } else None

  def getRecreatedState(version: Option[VersionTag] = None, digest: Option[ADDigest] = None): StateType = {
    val dir: File = EncryState.getStateDir(settings)
    dir.mkdirs()
    dir.listFiles.foreach(_.delete())

    {
      (version, digest) match {
        case (Some(_), Some(_)) if settings.node.stateMode.isDigest =>
          DigestState.create(version, digest, dir, settings.node)
        case _ => EncryState.readOrGenerate(settings, Some(self))
      }
    }.asInstanceOf[StateType]
      .ensuring(_.rootHash sameElements digest.getOrElse(EncryState.afterGenesisStateDigest), "State root is incorrect")
  }

  def restoreConsistentState(stateIn: StateType, history: EncryHistory): StateType =
    (stateIn.version, history.bestBlockOpt, stateIn) match {
      case (stateId, None, _) if stateId sameElements EncryState.genesisStateVersion =>
        info(s"State and history are both empty on startup")
        stateIn
      case (stateId, Some(block), _) if stateId sameElements block.id =>
        info(s"State and history have the same version ${Algos.encode(stateId)}, no recovery needed.")
        stateIn
      case (_, None, _) =>
        info(s"State and history are inconsistent. History is empty on startup, rollback state to genesis.")
        getRecreatedState()
      case (_, Some(bestBlock), _: DigestState) =>
        info(s"State and history are inconsistent. Going to switch state to version ${bestBlock.encodedId}")
        getRecreatedState(Some(VersionTag !@@ bestBlock.id), Some(bestBlock.header.stateRoot))
      case (stateId, Some(historyBestBlock), state: StateType@unchecked) =>
        val stateBestHeaderOpt = history.typedModifierById[EncryBlockHeader](ModifierId !@@ stateId)
        val (rollbackId, newChain) = history.getChainToHeader(stateBestHeaderOpt, historyBestBlock.header)
        info(s"State and history are inconsistent. Going to rollback to ${rollbackId.map(Algos.encode)} and " +
          s"apply ${newChain.length} modifiers")
        val startState = rollbackId.map(id => state.rollbackTo(VersionTag !@@ id).get)
          .getOrElse(getRecreatedState())
        val toApply = newChain.headers.map { h =>
          history.getBlock(h) match {
            case Some(fb) => fb
            case None => throw new Exception(s"Failed to get full block for header $h")
          }
        }
        toApply.foldLeft(startState) { (s, m) => s.applyModifier(m).get }
    }
}

object EncryNodeViewHolder {

  case class DownloadRequest(modifierTypeId: ModifierTypeId, modifierId: ModifierId) extends NodeViewHolderEvent

  case class CurrentView[HIS, MS, VL, MP](history: HIS, state: MS, vault: VL, pool: MP)

  object ReceivableMessages {

    case class GetNodeViewChanges(history: Boolean, state: Boolean, vault: Boolean, mempool: Boolean)

    case class GetDataFromCurrentView[HIS, MS, VL, MP, A](f: CurrentView[HIS, MS, VL, MP] => A)

    case class CompareViews(source: ConnectedPeer, modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId])

    case class BlocksFromLocalPersistence(blocks: Seq[EncryBlock])

    case class ModifiersFromRemote(modTypeId: ModifierTypeId, remoteObjects: Seq[Array[Byte]])

    case class LocallyGeneratedTransaction[P <: Proposition, EncryBaseTransaction](tx: EncryBaseTransaction)

    case class LocallyGeneratedModifier[EncryPersistentModifier <: PersistentNodeViewModifier](pmod: EncryPersistentModifier)

  }

  def props(): Props = settings.node.stateMode match {
    case StateMode.Digest => Props[EncryNodeViewHolder[DigestState]]
    case StateMode.Utxo => Props[EncryNodeViewHolder[UtxoState]]
  }
}