package encry.view

import java.io.File

import akka.actor.Props
import encry.EncryApp
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.header.{EncryBlockHeader, EncryBlockHeaderSerializer}
import encry.modifiers.history.block.payload.{EncryBlockPayload, EncryBlockPayloadSerializer}
import encry.modifiers.history.{ADProofSerializer, ADProofs}
import encry.modifiers.mempool.{EncryBaseTransaction, EncryTransactionSerializer}
import encry.modifiers.state.box.proposition.EncryProposition
import encry.settings.Algos
import encry.view.history.{EncryHistory, EncrySyncInfo}
import encry.view.mempool.EncryMempool
import encry.view.state.{DigestState, EncryState, StateMode, UtxoState}
import encry.view.wallet.EncryWallet
import encry.EncryApp.{P, networkController, settings, timeProvider}
import scorex.core._
import encry.network.NodeViewSynchronizer.ReceivableMessages.SuccessfulTransaction
import scorex.core
import scorex.core.consensus.History
import scorex.core.consensus.History.ProgressInfo
import scorex.core.serialization.Serializer
import scorex.core.transaction.state.{MinimalState, TransactionValidation}
import scorex.core.transaction.wallet.Vault
import scorex.core.transaction.{MemoryPool, Transaction}
import scorex.crypto.authds.ADDigest
import scorex.crypto.encode.Base58
import supertagged.@@

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class EncryNodeViewHolder[StateType <: EncryState[StateType]] extends NodeViewHolder {

  import NodeViewHolder.ReceivableMessages._
  import NodeViewHolder._
  import encry.network.NodeViewSynchronizer.ReceivableMessages._

  type NodeView = (HIS, MS, VL, MP)
  type HIS = EncryHistory
  type MS = StateType
  type VL = EncryWallet
  type MP = EncryMempool

  val modifierSerializers: Map[ModifierTypeId, Serializer[_ <: NodeViewModifier]] = Map(
    EncryBlockHeader.modifierTypeId -> EncryBlockHeaderSerializer,
    EncryBlockPayload.modifierTypeId -> EncryBlockPayloadSerializer,
    ADProofs.modifierTypeId -> ADProofSerializer,
    Transaction.ModifierTypeId -> EncryTransactionSerializer
  )

  var nodeView: NodeView = restoreState().getOrElse(genesisState)
  val modifiersCache: mutable.Map[scala.collection.mutable.WrappedArray.ofByte, EncryPersistentModifier] =
    mutable.Map[scala.collection.mutable.WrappedArray.ofByte, EncryPersistentModifier]()

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    super.preRestart(reason, message)
    reason.printStackTrace()
    System.exit(100)
  }

  override def postStop(): Unit = {
    log.warn("Stopping EncryNodeViewHolder")
    history().closeStorage()
    minimalState().closeStorage()
  }

  override def receive: Receive = {
    case ModifiersFromRemote(remote, modifierTypeId, remoteObjects) =>
      modifierSerializers.get(modifierTypeId) foreach { companion =>
        remoteObjects.flatMap(r => companion.parseBytes(r).toOption).foreach {
          case tx: EncryBaseTransaction@unchecked if tx.modifierTypeId == Transaction.ModifierTypeId => txModify(tx)
          case pmod: EncryPersistentModifier@unchecked =>
            if (history().contains(pmod) || modifiersCache.contains(key(pmod.id)))
              log.warn(s"Received modifier ${pmod.encodedId} that is already in history")
            else modifiersCache.put(key(pmod.id), pmod)
        }
        log.debug(s"Cache before(${modifiersCache.size}): ${modifiersCache.keySet.map(_.array).map(Base58.encode).mkString(",")}")
        var t: Option[EncryPersistentModifier] = None
        do {
          t = {
            modifiersCache.find { case (_, pmod) =>
              history().applicable(pmod)
            }.map { t =>
              val res = t._2
              modifiersCache.remove(t._1)
              res
            }
          }
          t.foreach(pmodModify)
        } while (t.isDefined)
        log.debug(s"Cache after(${modifiersCache.size}): ${modifiersCache.keySet.map(_.array).map(Base58.encode).mkString(",")}")
      }
    case lt: LocallyGeneratedTransaction[P, EncryBaseTransaction] => txModify(lt.tx)
    case lm: LocallyGeneratedModifier[EncryPersistentModifier] =>
      log.info(s"Got locally generated modifier ${lm.pmod.encodedId} of type ${lm.pmod.modifierTypeId}")
      pmodModify(lm.pmod)
    case GetDataFromCurrentView(f) => sender() ! f(CurrentView(history(), minimalState(), vault(), memoryPool()))
    case GetNodeViewChanges(history, state, vault, mempool) =>
      if (history) sender() ! ChangedHistory(nodeView._1.getReader)
      if (state) sender() ! ChangedState(nodeView._2.getReader)
      if (mempool) sender() ! ChangedMempool(nodeView._4.getReader)
    case CompareViews(peer, modifierTypeId, modifierIds) =>
      val ids: Seq[ModifierId] = modifierTypeId match {
        case typeId: ModifierTypeId if typeId == Transaction.ModifierTypeId => memoryPool().notIn(modifierIds)
        case _ => modifierIds.filterNot(mid => history().contains(mid) || modifiersCache.contains(key(mid)))
      }
      sender() ! RequestFromLocal(peer, modifierTypeId, ids)
    case a: Any => log.error("Strange input: " + a)
  }

  def history(): HIS = nodeView._1

  def minimalState(): MS = nodeView._2

  def vault(): VL = nodeView._3

  def memoryPool(): MP = nodeView._4

  def key(id: ModifierId): scala.collection.mutable.WrappedArray.ofByte = new mutable.WrappedArray.ofByte(id)

  def updateNodeView(updatedHistory: Option[HIS] = None,
                     updatedState: Option[MS] = None,
                     updatedVault: Option[VL] = None,
                     updatedMempool: Option[MP] = None): Unit = {
    val newNodeView: (HIS, MS, VL, MP) = (updatedHistory.getOrElse(history()),
      updatedState.getOrElse(minimalState()),
      updatedVault.getOrElse(vault()),
      updatedMempool.getOrElse(memoryPool()))
    if (updatedHistory.nonEmpty) context.system.eventStream.publish(ChangedHistory(newNodeView._1.getReader))
    if (updatedState.nonEmpty) context.system.eventStream.publish(ChangedState(newNodeView._2.getReader))
    if (updatedMempool.nonEmpty) context.system.eventStream.publish(ChangedMempool(newNodeView._4.getReader))
    nodeView = newNodeView
  }

  def extractTransactions(mod: EncryPersistentModifier): Seq[EncryBaseTransaction] = mod match {
    case tcm: TransactionsCarryingPersistentNodeViewModifier[P, EncryBaseTransaction] => tcm.transactions
    case _ => Seq()
  }

  //todo: this method causes delays in a block processing as it removes transactions from mempool and checks
  //todo: validity of remaining transactions in a synchronous way. Do this job async!
  def updateMemPool(blocksRemoved: Seq[EncryPersistentModifier], blocksApplied: Seq[EncryPersistentModifier], memPool: MP, state: MS): MP = {
    val rolledBackTxs: Seq[EncryBaseTransaction] = blocksRemoved.flatMap(extractTransactions)
    val appliedTxs: Seq[EncryBaseTransaction] = blocksApplied.flatMap(extractTransactions)
    memPool.putWithoutCheck(rolledBackTxs).filter { tx =>
      !appliedTxs.exists(t => t.id sameElements tx.id) && {
        state match {
          case v: TransactionValidation[P, EncryBaseTransaction] => v.validate(tx).isSuccess
          case _ => true
        }
      }
    }
  }

  def requestDownloads(pi: ProgressInfo[EncryPersistentModifier]): Unit =
    pi.toDownload.foreach { case (tid, id) => networkController ! DownloadRequest(tid, id) }

  def trimChainSuffix(suffix: IndexedSeq[EncryPersistentModifier], rollbackPoint: VersionTag): IndexedSeq[EncryPersistentModifier] = {
    val idx: Int = suffix.indexWhere(_.id.sameElements(rollbackPoint))
    if (idx == -1) IndexedSeq() else suffix.drop(idx)
  }

  @tailrec
  private def updateState(history: HIS,
                          state: MS,
                          progressInfo: ProgressInfo[EncryPersistentModifier],
                          suffixApplied: IndexedSeq[EncryPersistentModifier]): (HIS, Try[MS], Seq[EncryPersistentModifier]) = {
    case class UpdateInformation(history: HIS,
                                 state: MS,
                                 failedMod: Option[EncryPersistentModifier],
                                 alternativeProgressInfo: Option[ProgressInfo[EncryPersistentModifier]],
                                 suffix: IndexedSeq[EncryPersistentModifier])

    requestDownloads(progressInfo)
    val branchingPointOpt: Option[Array[Byte] @@ core.VersionTag.Tag] = progressInfo.branchPoint.map(VersionTag @@ _)
    val (stateToApplyTry: Try[MS], suffixTrimmed: IndexedSeq[EncryPersistentModifier]) = if (progressInfo.chainSwitchingNeeded) {
      if (!state.version.sameElements(branchingPointOpt))
        state.rollbackTo(branchingPointOpt.get) -> trimChainSuffix(suffixApplied, branchingPointOpt.get)
      else Success(state) -> IndexedSeq()
    } else Success(state) -> suffixApplied

    stateToApplyTry match {
      case Success(stateToApply) =>
        log.info(s"Rollback succeed: BranchPoint(${progressInfo.branchPoint.map(Base58.encode)})")
        context.system.eventStream.publish(RollbackSucceed(branchingPointOpt))
        val u0: UpdateInformation = UpdateInformation(history, stateToApply, None, None, suffixTrimmed)
        val uf: UpdateInformation = progressInfo.toApply.foldLeft(u0) { case (u, modToApply) =>
          if (u.failedMod.isEmpty) u.state.applyModifier(modToApply) match {
            case Success(stateAfterApply) =>
              val newHis: HIS = history.reportModifierIsValid(modToApply)
              context.system.eventStream.publish(SemanticallySuccessfulModifier(modToApply))
              UpdateInformation(newHis, stateAfterApply, None, None, u.suffix :+ modToApply)
            case Failure(e) =>
              val (newHis: HIS, newProgressInfo: ProgressInfo[EncryPersistentModifier]) = history.reportModifierIsInvalid(modToApply, progressInfo)
              context.system.eventStream.publish(SemanticallyFailedModification(modToApply, e))
              UpdateInformation(newHis, u.state, Some(modToApply), Some(newProgressInfo), u.suffix)
          }
          else u
        }
        uf.failedMod match {
          case Some(mod) => updateState(uf.history, uf.state, uf.alternativeProgressInfo.get, uf.suffix)
          case None => (uf.history, Success(uf.state), uf.suffix)
        }
      case Failure(e) =>
        log.error("Rollback failed: ", e)
        context.system.eventStream.publish(RollbackFailed(branchingPointOpt))
        EncryApp.forceStopApplication(500)
    }
  }

  def pmodModify(pmod: EncryPersistentModifier): Unit = if (!history().contains(pmod.id)) {
    context.system.eventStream.publish(StartingPersistentModifierApplication(pmod))
    log.info(s"Apply modifier ${pmod.encodedId} of type ${pmod.modifierTypeId} to nodeViewHolder")
    history().append(pmod) match {
      case Success((historyBeforeStUpdate, progressInfo)) =>
        log.debug(s"Going to apply modifications to the state: $progressInfo")
        context.system.eventStream.publish(SyntacticallySuccessfulModifier(pmod))
        context.system.eventStream.publish(NewOpenSurface(historyBeforeStUpdate.openSurfaceIds()))
        if (progressInfo.toApply.nonEmpty) {
          val (newHistory: HIS, newStateTry: Try[MS], blocksApplied: Seq[EncryPersistentModifier]) =
            updateState(historyBeforeStUpdate, minimalState(), progressInfo, IndexedSeq())
          newStateTry match {
            case Success(newMinState) =>
              val newMemPool: MP = updateMemPool(progressInfo.toRemove, blocksApplied, memoryPool(), newMinState)
              val newVault: VL = if (progressInfo.chainSwitchingNeeded)
                vault().rollback(VersionTag @@ progressInfo.branchPoint.get).get
              else vault()
              blocksApplied.foreach(newVault.scanPersistent)
              log.info(s"Persistent modifier ${pmod.encodedId} applied successfully")
              updateNodeView(Some(newHistory), Some(newMinState), Some(newVault), Some(newMemPool))
            case Failure(e) =>
              log.warn(s"Can`t apply persistent modifier (id: ${pmod.encodedId}, contents: $pmod) to minimal state", e)
              updateNodeView(updatedHistory = Some(newHistory))
              context.system.eventStream.publish(SemanticallyFailedModification(pmod, e))
          }
        } else {
          requestDownloads(progressInfo)
          updateNodeView(updatedHistory = Some(historyBeforeStUpdate))
        }
      case Failure(e) =>
        log.warn(s"Can`t apply persistent modifier (id: ${pmod.encodedId}, contents: $pmod) to history", e)
        context.system.eventStream.publish(SyntacticallyFailedModification(pmod, e))
    }
  } else log.warn(s"Trying to apply modifier ${pmod.encodedId} that's already in history")

  def txModify(tx: EncryBaseTransaction): Unit = memoryPool().put(tx) match {
    case Success(newPool) =>
      log.debug(s"Unconfirmed transaction $tx added to the memory pool")
      val newVault = vault().scanOffchain(tx)
      updateNodeView(updatedVault = Some(newVault), updatedMempool = Some(newPool))
      context.system.eventStream.publish(SuccessfulTransaction[EncryProposition, EncryBaseTransaction](tx))
    case Failure(e) =>
  }

  def genesisState: (EncryHistory, StateType, EncryWallet, EncryMempool) = {
    val stateDir: File = EncryState.getStateDir(settings)
    stateDir.mkdir()
    assert(stateDir.listFiles().isEmpty, s"Genesis directory $stateDir should always be empty")
    val state: StateType = {
      if (settings.node.stateMode.isDigest) EncryState.generateGenesisDigestState(stateDir, settings.node)
      else EncryState.generateGenesisUtxoState(stateDir, Some(self))._1
    }.asInstanceOf[StateType]
    val history: EncryHistory = EncryHistory.readOrGenerate(settings, timeProvider)
    val wallet: EncryWallet = EncryWallet.readOrGenerate(settings)
    val memPool: EncryMempool = EncryMempool.empty(settings, timeProvider)
    (history, state, wallet, memPool)
  }

  /**
    * Restore a local view during a node startup. If no any stored view found
    * (e.g. if it is a first launch of a node) None is to be returned
    */
  def restoreState(): Option[NodeView] = if (!EncryHistory.getHistoryDir(settings).listFiles.isEmpty) {
    val history: EncryHistory = EncryHistory.readOrGenerate(settings, timeProvider)
    val wallet: EncryWallet = EncryWallet.readOrGenerate(settings)
    val memPool: EncryMempool = EncryMempool.empty(settings, timeProvider)
    val state: StateType = restoreConsistentState(EncryState.readOrGenerate(settings, Some(self)).asInstanceOf[StateType], history)
    Some((history, state, wallet, memPool))
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

  def restoreConsistentState(stateIn: StateType, history: EncryHistory): StateType = Try {
    (stateIn.version, history.bestBlockOpt, stateIn) match {
      case (stateId, None, _) if stateId sameElements EncryState.genesisStateVersion =>
        log.debug("State and history are both empty on startup")
        stateIn
      case (stateId, Some(block), _) if stateId sameElements block.id =>
        log.debug(s"State and history have the same version ${Algos.encode(stateId)}, no recovery needed.")
        stateIn
      case (_, None, _) =>
        log.debug("State and history are inconsistent. History is empty on startup, rollback state to genesis.")
        getRecreatedState()
      case (_, Some(bestBlock), _: DigestState) =>
        log.debug(s"State and history are inconsistent. Going to switch state to version ${bestBlock.encodedId}")
        getRecreatedState(Some(VersionTag @@ bestBlock.id), Some(bestBlock.header.stateRoot))
      case (stateId, Some(historyBestBlock), state: StateType@unchecked) =>
        val stateBestHeaderOpt = history.typedModifierById[EncryBlockHeader](ModifierId @@ stateId)
        val (rollbackId, newChain) = history.getChainToHeader(stateBestHeaderOpt, historyBestBlock.header)
        log.debug(s"State and history are inconsistent. Going to rollback to ${rollbackId.map(Algos.encode)} and " +
          s"apply ${newChain.length} modifiers")
        val startState = rollbackId.map(id => state.rollbackTo(VersionTag @@ id).get)
          .getOrElse(getRecreatedState())
        val toApply = newChain.headers.map { h =>
          history.getBlock(h) match {
            case Some(fb) => fb
            case None => throw new Exception(s"Failed to get full block for header $h")
          }
        }
        toApply.foldLeft(startState) { (s, m) => s.applyModifier(m).get }
    }
  }.recoverWith { case e =>
    log.error("Failed to recover state.", e)
    EncryApp.forceStopApplication(500)
  }.get
}

object EncryNodeViewHolder {

  def props(): Props = settings.node.stateMode match {
    case digestType@StateMode.Digest => Props(classOf[EncryNodeViewHolder[DigestState]])
    case utxoType@StateMode.Utxo => Props(classOf[EncryNodeViewHolder[UtxoState]])
  }
}