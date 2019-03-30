package encry.view

import java.io.File

import HeaderProto.HeaderProtoMessage
import PayloadProto.PayloadProtoMessage
import TransactionProto.TransactionProtoMessage
import akka.actor.{Actor, ActorRef, Props}
import akka.pattern._
import com.typesafe.scalalogging.StrictLogging
import encry.EncryApp
import encry.EncryApp._
import encry.consensus.History.ProgressInfo
import encry.local.explorer.BlockListener.ChainSwitching
import encry.modifiers._
import encry.modifiers.history._
import encry.modifiers.mempool.{Transaction, TransactionProtoSerializer, TransactionSerializer}
import encry.modifiers.state.box.EncryProposition
import encry.network.AuxiliaryHistoryHolder.{Append, ReportModifierInvalid, ReportModifierValid}
import encry.network.DeliveryManager.{CheckModifiersWithQueueSize, FullBlockChainIsSynced, ModifiersFromNVH}
import encry.network.NodeViewSynchronizer.ReceivableMessages._
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.stats.StatsSender._
import encry.utils.CoreTaggedTypes.{ModifierId, ModifierTypeId, VersionTag}
import encry.view.EncryNodeViewHolder.ReceivableMessages._
import encry.view.EncryNodeViewHolder.{DownloadRequest, _}
import encry.view.history.EncryHistory
import encry.view.mempool.Mempool
import encry.view.state._
import encry.view.wallet.EncryWallet
import org.apache.commons.io.FileUtils
import org.encryfoundation.common.Algos
import org.encryfoundation.common.serialization.Serializer
import org.encryfoundation.common.transaction.Proposition
import org.encryfoundation.common.utils.TaggedTypes.ADDigest

import scala.annotation.tailrec
import scala.collection.{IndexedSeq, Seq, mutable}
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

class EncryNodeViewHolder[StateType <: EncryState[StateType]](auxHistoryHolder: ActorRef) extends Actor with StrictLogging {

  case class NodeView(history: EncryHistory, state: StateType, wallet: EncryWallet, mempool: Mempool)

  var applicationsSuccessful: Boolean = true
  var nodeView: NodeView = restoreState().getOrElse(genesisState)
  val modifierSerializers: Map[ModifierTypeId, Serializer[_ <: NodeViewModifier]] = Map(
    Header.modifierTypeId -> HeaderSerializer,
    Payload.modifierTypeId -> PayloadSerializer,
    ADProofs.modifierTypeId -> ADProofSerializer,
    Transaction.ModifierTypeId -> TransactionSerializer
  )

  if (settings.influxDB.isDefined) {
    context.system.scheduler.schedule(5.second, 5.second) {
      context.system.actorSelection("user/statsSender") !
        HeightStatistics(nodeView.history.bestHeaderHeight, nodeView.history.bestBlockHeight)
    }
  }

  if (settings.influxDB.isDefined) {
    context.system.scheduler.schedule(10.second, 1.second) {
      val txsInLastBlock: Int = nodeView.history.bestBlockOpt.map(x => x.payload.transactions.size).getOrElse(0)
      val txsInMempool: Int = nodeView.mempool.unconfirmed.values.size
      val diffBtw: Int = txsInMempool - txsInLastBlock

      context.system.actorSelection("user/statsSender") !
        TransactionsStatMessage(txsInLastBlock, nodeView.history.bestBlockHeight)

      context.system.actorSelection("user/statsSender") ! MempoolStat(txsInMempool)

      context.system.actorSelection("user/statsSender") ! DiffBtwMempoolAndLastBlockTxs(diffBtw)
    }
  }

//  context.system.scheduler.schedule(5.second, settings.network.modifierDeliverTimeCheck){
//    println(s"Trigger CheckModifiersToDownload from context.system.scheduler.schedule ON NVH")
//    self ! CheckModifiersToDownload
//  }

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    reason.printStackTrace()
    System.exit(100)
  }

  system.scheduler.schedule(5.seconds, 10.seconds)(println(s"Modifiers cache from NVH: ${ModifiersCache.size}"))

  override def postStop(): Unit = {
    logger.warn(s"Stopping EncryNodeViewHolder")
    nodeView.history.closeStorage()
    nodeView.state.closeStorage()
  }

  override def receive: Receive = {
    case ModifiersFromRemote(modifierTypeId, modifiers) => modifierTypeId match {
      case Payload.modifierTypeId => modifiers.foreach { bytes =>
        Try(PayloadProtoSerializer.fromProto(PayloadProtoMessage.parseFrom(bytes)).foreach { payload =>
          if (nodeView.history.contains(payload.id) || ModifiersCache.contains(key(payload.id)))
            logger.warn(s"Received modifier ${payload.encodedId} that is already in history")
          else ModifiersCache.put(key(payload.id), payload, nodeView.history)
        })
      }
        logger.debug(s"Cache before(${ModifiersCache.size})")
        computeApplications()
        self ! CheckModifiersToDownload
        logger.debug(s"Cache after(${ModifiersCache.size})")
      case Header.modifierTypeId =>
        modifiers.foreach { bytes =>
          Try(HeaderProtoSerializer.fromProto(HeaderProtoMessage.parseFrom(bytes)).foreach { header =>
            if (nodeView.history.contains(header.id) || ModifiersCache.contains(key(header.id)))
              logger.warn(s"Received modifier ${header.encodedId} that is already in history")
            else ModifiersCache.put(key(header.id), header, nodeView.history)
            if (settings.influxDB.isDefined && nodeView.history.isFullChainSynced) {
              header match {
                case h: Header => context.system.actorSelection("user/statsSender") ! TimestampDifference(timeProvider.estimatedTime - h.timestamp)
                case _ =>
              }
            }
          })
        }
        logger.debug(s"Cache before(${ModifiersCache.size})")
        computeApplications()
        self ! CheckModifiersToDownload
        logger.debug(s"Cache after(${ModifiersCache.size})")
      case Transaction.ModifierTypeId => modifiers.foreach { bytes =>
        Try(TransactionProtoSerializer.fromProto(TransactionProtoMessage.parseFrom(bytes)).foreach(tx => txModify(tx)))
      }
    }
    case lt: LocallyGeneratedTransaction[EncryProposition, Transaction]@unchecked => txModify(lt.tx)
    case lm: LocallyGeneratedModifier[EncryPersistentModifier]@unchecked =>
      logger.info(s"Got locally generated modifier ${lm.pmod.encodedId} of type ${lm.pmod.modifierTypeId}")
      pmodModify(lm.pmod, true)
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
        case Transaction.ModifierTypeId => nodeView.mempool.notRequested(modifierIds)
        case _ => modifierIds.filterNot(mid => nodeView.history.contains(mid) || ModifiersCache.contains(key(mid)))
      }
      logger.info(s"\n\n\nNVH GOT CompareViews. Current cache is ${ModifiersCache.size}. Requested mods -> ${modifierIds.size}. Filtered mpds -> ${ids.size}\n\n\n")

      if (ids.nonEmpty) sender() ! RequestFromLocal(peer, modifierTypeId, ids)
    case CheckModifiersToDownload=>
      println(s"GOT CheckModifiersToDownload on NVH")
      val mods: Seq[(ModifierTypeId, ModifierId)] =
        nodeView.history.modifiersToDownload(settings.network.networkChunkSize,
          ModifiersCache.cache.map(x => ModifierId @@ x._1.toArray))
      println(mods.size + " MODIFIERS ON NVH")
      logger.info(mods.size + " MODIFIERS ON NVH")
      val fm: Seq[(ModifierTypeId, ModifierId)] = mods.filterNot {  case (_, id) =>
        val a = ModifiersCache.contains(key(id))
        //logger.info(s"$a")
        a
      }
      println(fm.size + " FILTERED MODIFIERS ON NVH")
      logger.info(fm.size + " FILTERED MODIFIERS ON NVH")
      nodeViewSynchronizer ! ModifiersFromNVH(fm)
    case a: Any => logger.error(s"Strange input: $a")
  }

  //todo refactor loop
  def computeApplications(): Unit = {
    val mods = ModifiersCache.popCandidate(nodeView.history)
    if (mods.nonEmpty) {
      mods.foreach(mod => pmodModify(mod))
      computeApplications()
    }
    else Unit
  }

  def key(id: ModifierId): mutable.WrappedArray.ofByte = new mutable.WrappedArray.ofByte(id)

  def updateNodeView(updatedHistory: Option[EncryHistory] = None,
                     updatedState: Option[StateType] = None,
                     updatedVault: Option[EncryWallet] = None,
                     updatedMempool: Option[Mempool] = None): Unit = {
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
    case tcm: TransactionsCarryingPersistentNodeViewModifier[EncryProposition, Transaction]@unchecked => tcm.transactions
    case _ => Seq()
  }

  def updateMemPool(blocksRemoved: Seq[EncryPersistentModifier], blocksApplied: Seq[EncryPersistentModifier],
                    memPool: Mempool, state: StateType): Mempool = {
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

  def requestDownloads(pi: ProgressInfo[EncryPersistentModifier], previousModifier: Option[ModifierId] = None): Unit =
    pi.toDownload.foreach { case (tid, id) => nodeViewSynchronizer ! DownloadRequest(tid, id, previousModifier) }


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

    requestDownloads(progressInfo, None)
    val branchingPointOpt: Option[VersionTag] = progressInfo.branchPoint.map(VersionTag !@@ _)
    val (stateToApplyTry: Try[StateType], suffixTrimmed: IndexedSeq[EncryPersistentModifier]@unchecked) =
      if (progressInfo.chainSwitchingNeeded) {
        branchingPointOpt.map { branchPoint =>
          if (!state.version.sameElements(branchPoint))
            state.rollbackTo(branchPoint) -> trimChainSuffix(suffixApplied, ModifierId !@@ branchPoint)
          else Success(state) -> IndexedSeq()
        }.getOrElse(Failure(new Exception("Trying to rollback when branchPoint is empty.")))
      } else Success(state) -> suffixApplied

    stateToApplyTry match {
      case Success(stateToApply) =>
        context.system.eventStream.publish(RollbackSucceed(branchingPointOpt))
        val u0: UpdateInformation = UpdateInformation(history, stateToApply, None, None, suffixTrimmed)
        val uf: UpdateInformation = progressInfo.toApply.foldLeft(u0) { case (u, modToApply) =>
          if (u.failedMod.isEmpty) u.state.applyModifier(modToApply) match {
            case Success(stateAfterApply) =>
              modToApply match {
                case block: Block if settings.influxDB.isDefined =>
                  context.system.actorSelection("user/statsSender") ! TxsInBlock(block.transactions.size)
                case mod =>
              }
              val newHis: EncryHistory = history.reportModifierIsValid(modToApply)
              auxHistoryHolder ! ReportModifierValid(modToApply)
              context.system.eventStream.publish(SemanticallySuccessfulModifier(modToApply))
              if (settings.influxDB.isDefined) context.system
                .actorSelection("user/statsSender") ! NewBlockAppended(false, true)
              UpdateInformation(newHis, stateAfterApply, None, None, u.suffix :+ modToApply)
            case Failure(e) =>
              val (newHis: EncryHistory, newProgressInfo: ProgressInfo[EncryPersistentModifier]) =
                history.reportModifierIsInvalid(modToApply, progressInfo)
              auxHistoryHolder ! ReportModifierInvalid(modToApply, progressInfo)
              if (settings.influxDB.isDefined) context.system
                .actorSelection("user/statsSender") ! NewBlockAppended(false, false)
              nodeViewSynchronizer ! SemanticallyFailedModification(modToApply, e)
              UpdateInformation(newHis, u.state, Some(modToApply), Some(newProgressInfo), u.suffix)
          } else u
        }
        uf.failedMod match {
          case Some(_) => updateState(uf.history, uf.state, uf.alternativeProgressInfo.get, uf.suffix)
          case None => (uf.history, Success(uf.state), uf.suffix)
        }
      case Failure(e) =>
        logger.error(s"Rollback failed: $e")
        context.system.eventStream.publish(RollbackFailed(branchingPointOpt))
        EncryApp.forceStopApplication(500)
    }
  }

  def pmodModify(pmod: EncryPersistentModifier, isLocallyGenerated: Boolean = false): Unit = if (!nodeView.history.contains(pmod.id)) {
    logger.info(s"Apply modifier ${pmod.encodedId} of type ${pmod.modifierTypeId} to nodeViewHolder")
    if (settings.influxDB.isDefined) context.system
      .actorSelection("user/statsSender") !
      StartApplyingModif(pmod.id, pmod.modifierTypeId, System.currentTimeMillis())
    auxHistoryHolder ! Append(pmod)
    nodeView.history.append(pmod) match {
      case Success((historyBeforeStUpdate, progressInfo)) =>
        if (settings.influxDB.isDefined)
          context.system.actorSelection("user/statsSender") ! EndOfApplyingModif(pmod.id)
        logger.info(s"Going to apply modifications to the state: $progressInfo")
        if (progressInfo.toApply.nonEmpty) {
          val startPoint: Long = System.currentTimeMillis()
          val (newHistory: EncryHistory, newStateTry: Try[StateType], blocksApplied: Seq[EncryPersistentModifier]) =
            updateState(historyBeforeStUpdate, nodeView.state, progressInfo, IndexedSeq())
          if (settings.influxDB.isDefined)
            context.actorSelection("/user/statsSender") ! StateUpdating(System.currentTimeMillis() - startPoint)
          newStateTry match {
            case Success(newMinState) =>
              val newMemPool: Mempool =
                updateMemPool(progressInfo.toRemove, blocksApplied, nodeView.mempool, newMinState)
              if (progressInfo.chainSwitchingNeeded)
                nodeView.wallet.rollback(VersionTag !@@ progressInfo.branchPoint.get).get
              else nodeView.wallet
              blocksApplied.foreach(nodeView.wallet.scanPersistent)
              logger.info(s"Persistent modifier ${pmod.encodedId} applied successfully")
              if (progressInfo.chainSwitchingNeeded)
                context.actorSelection("/user/blockListener") !
                  ChainSwitching(progressInfo.toRemove.map(_.id))
              if (settings.influxDB.isDefined)
                newHistory.bestHeaderOpt.foreach(header =>
                  context.actorSelection("/user/statsSender") !
                    BestHeaderInChain(header, System.currentTimeMillis()))
              if (newHistory.isFullChainSynced) {
                logger.info(s"blockchain is synced on nvh on height ${newHistory.bestHeaderHeight}!")
                Seq(nodeViewSynchronizer, miner).foreach(_ ! FullBlockChainIsSynced)
              }
              updateNodeView(Some(newHistory), Some(newMinState), Some(nodeView.wallet), Some(newMemPool))
            case Failure(e) =>
              logger.warn(s"Can`t apply persistent modifier (id: ${pmod.encodedId}, contents: $pmod) " +
                s"to minimal state because of: $e")
              updateNodeView(updatedHistory = Some(newHistory))
              nodeViewSynchronizer ! SemanticallyFailedModification(pmod, e)
          }
        } else {
          if (settings.influxDB.isDefined && pmod.modifierTypeId == Header.modifierTypeId) context.system
            .actorSelection("user/statsSender") ! NewBlockAppended(true, true)
          if (!isLocallyGenerated) requestDownloads(progressInfo, Some(pmod.id))
          updateNodeView(updatedHistory = Some(historyBeforeStUpdate))
        }
      case Failure(e) =>
        if (settings.influxDB.isDefined && pmod.modifierTypeId == Header.modifierTypeId) context.system
          .actorSelection("user/statsSender") ! NewBlockAppended(true, false)
        logger.warn(s"Can`t apply persistent modifier (id: ${pmod.encodedId}, contents: $pmod)" +
          s" to history caused $e")
        nodeViewSynchronizer ! SyntacticallyFailedModification(pmod, e)
    }
  } else logger.warn(s"Trying to apply modifier ${pmod.encodedId} that's already in history.")

  def txModify(tx: Transaction): Unit = nodeView.mempool.put(tx) match {
    case Success(newPool) =>
      //TODO for what we update wallet?
      updateNodeView(updatedVault = Some(nodeView.wallet), updatedMempool = Some(newPool))
      nodeViewSynchronizer ! SuccessfulTransaction[EncryProposition, Transaction](tx)
    case Failure(e) => logger.warn(s"Failed to put tx ${tx.id} to mempool" +
      s" with exception ${e.getLocalizedMessage}")
  }

  def genesisState: NodeView = {
    val stateDir: File = EncryState.getStateDir(settings)
    stateDir.mkdir()
    assert(stateDir.listFiles().isEmpty, s"Genesis directory $stateDir should always be empty")
    val state: StateType = {
      if (settings.node.stateMode.isDigest) EncryState.generateGenesisDigestState(stateDir, settings)
      else EncryState.generateGenesisUtxoState(stateDir, Some(self), settings, influxRef)
    }.asInstanceOf[StateType]
    val history: EncryHistory = EncryHistory.readOrGenerate(settings, timeProvider)
    val wallet: EncryWallet = EncryWallet.readOrGenerate(settings)
    val memPool: Mempool = Mempool.empty(settings, timeProvider, system)
    NodeView(history, state, wallet, memPool)
  }

  def restoreState(): Option[NodeView] = if (!EncryHistory.getHistoryObjectsDir(settings).listFiles.isEmpty)
    try {
      val history: EncryHistory = EncryHistory.readOrGenerate(settings, timeProvider)
      val wallet: EncryWallet = EncryWallet.readOrGenerate(settings)
      val memPool: Mempool = Mempool.empty(settings, timeProvider, system)
      val state: StateType =
        restoreConsistentState(EncryState.readOrGenerate(settings, Some(self), influxRef).asInstanceOf[StateType], history)
      Some(NodeView(history, state, wallet, memPool))
    } catch {
      case ex: Throwable =>
        logger.info(s"${ex.getMessage} during state restore. Recover from Modifiers holder!")
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
          DigestState.create(version, digest, dir, settings)
        case _ => EncryState.readOrGenerate(settings, Some(self), influxRef)
      }
    }.asInstanceOf[StateType]
      .ensuring(_.rootHash sameElements digest.getOrElse(EncryState.afterGenesisStateDigest), "State root is incorrect")
  }

  def restoreConsistentState(stateIn: StateType, history: EncryHistory): StateType =
    (stateIn.version, history.bestBlockOpt, stateIn) match {
      case (stateId, None, _) if stateId sameElements EncryState.genesisStateVersion =>
        logger.info(s"State and history are both empty on startup")
        stateIn
      case (stateId, Some(block), _) if stateId sameElements block.id =>
        logger.info(s"State and history have the same version ${Algos.encode(stateId)}, no recovery needed.")
        stateIn
      case (_, None, _) =>
        logger.info(s"State and history are inconsistent." +
          s" History is empty on startup, rollback state to genesis.")
        getRecreatedState()
      case (_, Some(bestBlock), _: DigestState) =>
        logger.info(s"State and history are inconsistent." +
          s" Going to switch state to version ${bestBlock.encodedId}")
        getRecreatedState(Some(VersionTag !@@ bestBlock.id), Some(bestBlock.header.stateRoot))
      case (stateId, Some(historyBestBlock), state: StateType@unchecked) =>
        val stateBestHeaderOpt = history.typedModifierById[Header](ModifierId !@@ stateId)
        val (rollbackId, newChain) = history.getChainToHeader(stateBestHeaderOpt, historyBestBlock.header)
        logger.info(s"State and history are inconsistent." +
          s" Going to rollback to ${rollbackId.map(Algos.encode)} and " +
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

  case class DownloadRequest(modifierTypeId: ModifierTypeId,
                             modifierId: ModifierId,
                             previousModifier: Option[ModifierId] = None) extends NodeViewHolderEvent

  case class CurrentView[HIS, MS, VL, MP](history: HIS, state: MS, vault: VL, pool: MP)

  object ReceivableMessages {

    case class GetNodeViewChanges(history: Boolean, state: Boolean, vault: Boolean, mempool: Boolean)

    case class GetDataFromCurrentView[HIS, MS, VL, MP, A](f: CurrentView[HIS, MS, VL, MP] => A)

    case class CompareViews(source: ConnectedPeer, modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId])

    case class ModifiersFromRemote(modTypeId: ModifierTypeId, remoteObjects: Seq[Array[Byte]])

    case class LocallyGeneratedTransaction[P <: Proposition, EncryBaseTransaction](tx: EncryBaseTransaction)

    case class LocallyGeneratedModifier[EncryPersistentModifier <: PersistentNodeViewModifier]
    (pmod: EncryPersistentModifier)

  }

  def props(auxHistoryHolder: ActorRef): Props = settings.node.stateMode match {
    case StateMode.Digest => Props(new EncryNodeViewHolder[DigestState](auxHistoryHolder))
    case StateMode.Utxo => Props(new EncryNodeViewHolder[UtxoState](auxHistoryHolder))
  }
}