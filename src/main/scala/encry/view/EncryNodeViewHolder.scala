package encry.view

import java.io.File

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern._
import encry.EncryApp
import encry.EncryApp._
import encry.consensus.History
import encry.consensus.History.ProgressInfo
import encry.local.explorer.BlockListener.ChainSwitching
import encry.local.miner.Miner.DisableMining
import encry.modifiers._
import encry.modifiers.history._
import encry.modifiers.mempool.{Transaction, TransactionSerializer}
import encry.modifiers.state.box.EncryProposition
import encry.network.NodeViewSynchronizer.ReceivableMessages._
import encry.network.DeliveryManager.FullBlockChainSynced
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.stats.StatsSender._
import encry.utils.CoreTaggedTypes.{ModifierId, ModifierTypeId, VersionTag}
import encry.utils.Logging
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
import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

class EncryNodeViewHolder[StateType <: EncryState[StateType]] extends Actor with Logging {

  implicit val ec: ExecutionContextExecutor = context.dispatcher

  case class NodeView(state: StateType, wallet: EncryWallet, mempool: Mempool)

  var (nodeView: NodeView, history: EncryHistory) = restoreState().getOrElse(genesisState)
  val modifierSerializers: Map[ModifierTypeId, Serializer[_ <: NodeViewModifier]] = Map(
    Header.modifierTypeId -> HeaderSerializer,
    Payload.modifierTypeId -> PayloadSerializer,
    ADProofs.modifierTypeId -> ADProofSerializer,
    Transaction.ModifierTypeId -> TransactionSerializer
  )

  val nodeHistoryHolder: ActorRef = context.actorOf(Props(new NodeHistoryHolder(history, settings)).withDispatcher("nhh-dispatcher"))

  if (settings.influxDB.isDefined) {
    context.system.scheduler.schedule(10.second, 1.second) {
      context.system.actorSelection("user/statsSender") ! MempoolStat(nodeView.mempool.unconfirmed.values.size)
    }
  }

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    reason.printStackTrace()
    System.exit(100)
  }

  override def postStop(): Unit = {
    logWarn(s"Stopping EncryNodeViewHolder")
    nodeView.state.closeStorage()
  }

  override def receive: Receive = {
    case HistoryUpdate(historyUpdate, pmod) => processUpdatedHistory(historyUpdate, pmod)
    case msg: ModifiersFromRemote => nodeHistoryHolder ! msg
    case TxForTxModify(tx) => txModify(tx)
    case lt: LocallyGeneratedTransaction[EncryProposition, Transaction] => txModify(lt.tx)
    case lm: LocallyGeneratedModifier[EncryPersistentModifier] =>
      logInfo(s"Got locally generated modifier ${lm.pmod.encodedId} of type ${lm.pmod.modifierTypeId}")
      nodeHistoryHolder ! lm.pmod
    case msg: GetFromCurrentHistory[Any] => nodeHistoryHolder.forward(msg)
    case GetDataFromCurrentView(f) =>
      val result = f(CurrentView(nodeView.state, nodeView.wallet, nodeView.mempool))
      result match {
        case resultFuture: Future[_] => resultFuture.pipeTo(sender())
        case _ => sender() ! result
      }
    case msg@GetNodeViewChanges(history, state, vault, mempool) =>
      if (history) nodeHistoryHolder.forward(msg)
      if (state) sender() ! ChangedState(nodeView.state)
      if (mempool) sender() ! ChangedMempool(nodeView.mempool)
    case CompareViews(peer, modifierTypeId, modifierIds) if modifierTypeId == Transaction.ModifierTypeId =>
      sender() ! RequestFromLocal(peer, modifierTypeId, nodeView.mempool.notIn(modifierIds))
    case msg: CompareViews => nodeHistoryHolder.forward(msg)
    case a: Any =>
      logError(s"Strange input: $a")
  }

  def updateNodeView(updatedHistory: Option[EncryHistory] = None,
                     updatedState: Option[StateType] = None,
                     updatedVault: Option[EncryWallet] = None,
                     updatedMempool: Option[Mempool] = None): Unit = {
    updatedHistory.foreach(nodeHistoryHolder ! HistoryUpdateFromNVH(_, true))
    val newNodeView: NodeView = NodeView(updatedState.getOrElse(nodeView.state),
      updatedVault.getOrElse(nodeView.wallet),
      updatedMempool.getOrElse(nodeView.mempool))
    if (updatedState.nonEmpty) context.system.eventStream.publish(ChangedState(newNodeView.state))
    if (updatedMempool.nonEmpty) context.system.eventStream.publish(ChangedMempool(newNodeView.mempool))
    nodeView = newNodeView
  }

  def extractTransactions(mod: EncryPersistentModifier): Seq[Transaction] = mod match {
    case tcm: TransactionsCarryingPersistentNodeViewModifier[EncryProposition, Transaction] => tcm.transactions
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
              modToApply match {
                case block: Block if settings.influxDB.isDefined =>
                  context.system.actorSelection("user/statsSender") ! TxsInBlock(block.transactions.size)
                case mod =>
              }
              val newHis: EncryHistory = history.reportModifierIsValid(modToApply)
              context.system.eventStream.publish(SemanticallySuccessfulModifier(modToApply))
              if (settings.influxDB.isDefined) context.system
                .actorSelection("user/statsSender") ! NewBlockAppended(false, true)
              UpdateInformation(newHis, stateAfterApply, None, None, u.suffix :+ modToApply)
            case Failure(e) =>
              val (newHis: EncryHistory, newProgressInfo: ProgressInfo[EncryPersistentModifier]) =
                history.reportModifierIsInvalid(modToApply, progressInfo)
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
        logError(s"Rollback failed: $e")
        context.system.eventStream.publish(RollbackFailed(branchingPointOpt))
        EncryApp.forceStopApplication(500)
    }
  }

  def processUpdatedHistory(historyUpdate: Try[(EncryHistory, History.ProgressInfo[EncryPersistentModifier])],
                            pmod: EncryPersistentModifier): Unit = historyUpdate match {
    case Success((historyBeforeStUpdate, progressInfo)) =>
      if (settings.influxDB.isDefined)
        context.system.actorSelection("user/statsSender") ! EndOfApplyingModif(pmod.id)
      logInfo(s"Going to apply modifications to the state: $progressInfo")
      nodeViewSynchronizer ! SyntacticallySuccessfulModifier(pmod)
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
            val newVault: EncryWallet = if (progressInfo.chainSwitchingNeeded)
              nodeView.wallet.rollback(VersionTag !@@ progressInfo.branchPoint.get).get
            else nodeView.wallet
            blocksApplied.foreach(newVault.scanPersistent)
            logInfo(s"Persistent modifier ${pmod.encodedId} applied successfully")
            if (progressInfo.chainSwitchingNeeded)
              context.actorSelection("/user/blockListener") !
                ChainSwitching(progressInfo.toRemove.map(_.id))
            if (settings.influxDB.isDefined)
              newHistory.bestHeaderOpt.foreach(header =>
                context.actorSelection("/user/statsSender") !
                  BestHeaderInChain(header, System.currentTimeMillis()))
            if (newHistory.isFullChainSynced)
              Seq(nodeViewSynchronizer, miner).foreach(_ ! FullBlockChainSynced)
            else miner ! DisableMining
            updateNodeView(Some(newHistory), Some(newMinState), Some(newVault), Some(newMemPool))
          case Failure(e) =>
            logWarn(s"Can`t apply persistent modifier (id: ${pmod.encodedId}, contents: $pmod) " +
              s"to minimal state because of: $e")
            updateNodeView(updatedHistory = Some(newHistory))
            nodeViewSynchronizer ! SemanticallyFailedModification(pmod, e)
        }
      } else {
        if (settings.influxDB.isDefined && pmod.modifierTypeId == Header.modifierTypeId) context.system
          .actorSelection("user/statsSender") ! NewBlockAppended(true, true)
        requestDownloads(progressInfo)
        updateNodeView(updatedHistory = Some(historyBeforeStUpdate))
      }
    case Failure(e) =>
      if (settings.influxDB.isDefined && pmod.modifierTypeId == Header.modifierTypeId) context.system
        .actorSelection("user/statsSender") ! NewBlockAppended(true, false)
      logWarn(s"Can`t apply persistent modifier (id: ${pmod.encodedId}, contents: $pmod)" +
        s" to history caused $e")
      nodeViewSynchronizer ! SyntacticallyFailedModification(pmod, e)
  }

  def txModify(tx: Transaction): Unit = nodeView.mempool.put(tx) match {
    case Success(newPool) =>
      val newVault: EncryWallet = nodeView.wallet.scanOffchain(tx)
      updateNodeView(updatedVault = Some(newVault), updatedMempool = Some(newPool))
      nodeViewSynchronizer ! SuccessfulTransaction[EncryProposition, Transaction](tx)
    case Failure(e) => logWarn(s"Failed to put tx ${tx.id} to mempool" +
      s" with exception ${e.getLocalizedMessage}")
  }

  def genesisState: (NodeView, EncryHistory) = {
    val stateDir: File = EncryState.getStateDir(settings)
    stateDir.mkdir()
    assert(stateDir.listFiles().isEmpty, s"Genesis directory $stateDir should always be empty")
    val state: StateType = {
      if (settings.node.stateMode.isDigest) EncryState.generateGenesisDigestState(stateDir, settings.node)
      else EncryState.generateGenesisUtxoState(stateDir, Some(self))
    }.asInstanceOf[StateType]
    val history: EncryHistory = EncryHistory.readOrGenerate(settings, timeProvider)
    val wallet: EncryWallet = EncryWallet.readOrGenerate(settings)
    val memPool: Mempool = Mempool.empty(settings, timeProvider, system)
    (NodeView(state, wallet, memPool), history)
  }

  def restoreState(): Option[(NodeView, EncryHistory)] = if (!EncryHistory.getHistoryObjectsDir(settings).listFiles.isEmpty)
    try {
      val history: EncryHistory = EncryHistory.readOrGenerate(settings, timeProvider)
      val wallet: EncryWallet = EncryWallet.readOrGenerate(settings)
      val memPool: Mempool = Mempool.empty(settings, timeProvider, system)
      val state: StateType =
        restoreConsistentState(EncryState.readOrGenerate(settings, Some(self)).asInstanceOf[StateType], history)
      Some((NodeView(state, wallet, memPool), history))
    } catch {
      case ex: Throwable =>
        logInfo(s"${ex.getMessage} during state restore. Recover from Modifiers holder!")
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
        logInfo(s"State and history are both empty on startup")
        stateIn
      case (stateId, Some(block), _) if stateId sameElements block.id =>
        logInfo(s"State and history have the same version ${Algos.encode(stateId)}, no recovery needed.")
        stateIn
      case (_, None, _) =>
        logInfo(s"State and history are inconsistent." +
          s" History is empty on startup, rollback state to genesis.")
        getRecreatedState()
      case (_, Some(bestBlock), _: DigestState) =>
        logInfo(s"State and history are inconsistent." +
          s" Going to switch state to version ${bestBlock.encodedId}")
        getRecreatedState(Some(VersionTag !@@ bestBlock.id), Some(bestBlock.header.stateRoot))
      case (stateId, Some(historyBestBlock), state: StateType@unchecked) =>
        val stateBestHeaderOpt = history.typedModifierById[Header](ModifierId !@@ stateId)
        val (rollbackId, newChain) = history.getChainToHeader(stateBestHeaderOpt, historyBestBlock.header)
        logInfo(s"State and history are inconsistent." +
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

  case class DownloadRequest(modifierTypeId: ModifierTypeId, modifierId: ModifierId) extends NodeViewHolderEvent

  case class CurrentView[MS, VL, MP](state: MS, vault: VL, pool: MP)

  object ReceivableMessages {

    case class GetNodeViewChanges(history: Boolean, state: Boolean, vault: Boolean, mempool: Boolean)

    case class GetDataFromCurrentView[MS, VL, MP, A](f: CurrentView[MS, VL, MP] => A)

    case class GetFromCurrentHistory[A](f: EncryHistory => A)

    case class CompareViews(source: ConnectedPeer, modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId])

    case class ModifiersFromRemote(modTypeId: ModifierTypeId, remoteObjects: Seq[Array[Byte]])

    case class LocallyGeneratedTransaction[P <: Proposition, EncryBaseTransaction](tx: EncryBaseTransaction)

    case class LocallyGeneratedModifier[EncryPersistentModifier <: PersistentNodeViewModifier]
    (pmod: EncryPersistentModifier)

    case class RequestChangedHistory(requestSender: ActorRef)

  }

  def props(): Props = settings.node.stateMode match {
    case StateMode.Digest => Props[EncryNodeViewHolder[DigestState]]
    case StateMode.Utxo => Props[EncryNodeViewHolder[UtxoState]]
  }
}