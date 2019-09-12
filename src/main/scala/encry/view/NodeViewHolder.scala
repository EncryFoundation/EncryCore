package encry.view

import java.io.File

import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props}
import akka.dispatch.{PriorityGenerator, UnboundedStablePriorityMailbox}
import akka.pattern._
import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import encry.EncryApp
import encry.EncryApp.{miner, nodeViewSynchronizer, settings, timeProvider}
import encry.consensus.HistoryConsensus.ProgressInfo
import encry.network.DeliveryManager.FullBlockChainIsSynced
import encry.network.NodeViewSynchronizer.ReceivableMessages._
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.stats.StatsSender._
import encry.utils.CoreTaggedTypes.VersionTag
import encry.view.NodeViewErrors.ModifierApplyError.HistoryApplyError
import encry.view.NodeViewHolder.ReceivableMessages._
import encry.view.NodeViewHolder._
import encry.view.actors.HistoryApplicator
import encry.view.actors.HistoryApplicator.ModifierFromNetwork
import encry.view.history.History
import encry.view.mempool.MemoryPool.RolledBackTransactions
import encry.view.state._
import encry.view.wallet.EncryWallet
import org.apache.commons.io.FileUtils
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history._
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ADDigest, ModifierId, ModifierTypeId}

import scala.collection.{IndexedSeq, Seq, mutable}
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.util.{Failure, Success, Try}

class NodeViewHolder(memoryPoolRef: ActorRef,
                     influxRef: Option[ActorRef],
                     dataHolder: ActorRef) extends Actor with StrictLogging with AutoCloseable {

  implicit val exCon: ExecutionContextExecutor = context.dispatcher

  case class NodeView(history: History, state: UtxoState, wallet: EncryWallet)

  var applicationsSuccessful: Boolean = true
  var nodeView: NodeView = restoreState().getOrElse(genesisState)

  dataHolder ! UpdatedHistory(nodeView.history)
  dataHolder ! ChangedState(nodeView.state)

  influxRef.foreach(ref => context.system.scheduler.schedule(5.second, 5.second) {
    ref ! HeightStatistics(nodeView.history.getBestHeaderHeight, nodeView.history.getBestBlockHeight)
  })

  val historyApplicator: ActorRef =
    context.system.actorOf(HistoryApplicator.props(nodeView.history, settings, nodeView.state)
      .withDispatcher("history-applicator-dispatcher"), "historyApplicator")

  override def preStart(): Unit = logger.info(s"Node view holder started.")

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    reason.printStackTrace()
    System.exit(100)
  }

  context.system.scheduler.schedule(1.seconds, 10.seconds)(logger.debug(s"Modifiers cache from NVH: " +
    s"${ModifiersCache.size}. Elems: ${ModifiersCache.cache.keys.map(key => Algos.encode(key.toArray)).mkString(",")}"))

  override def postStop(): Unit = {
    logger.warn(s"Stopping NodeViewHolder...")
    nodeView.history.closeStorage()
  }

  override def receive: Receive = {
    case ModifierFromRemote(mod) =>
      historyApplicator ! ModifierFromNetwork(mod)
//      val isInHistory: Boolean = nodeView.history.isModifierDefined(mod.id)
//      val isInCache: Boolean = ModifiersCache.contains(key(mod.id))
//      if (isInHistory || isInCache)
//        logger.debug(s"Received modifier of type: ${mod.modifierTypeId}  ${Algos.encode(mod.id)} " +
//          s"can't be placed into cache cause of: inCache: ${!isInCache}.")
//      else ModifiersCache.put(key(mod.id), mod, nodeView.history)
//      computeApplications()

    case lm: LocallyGeneratedModifier =>
      logger.debug(s"Start processing LocallyGeneratedModifier message on NVH.")
      val startTime = System.currentTimeMillis()
      logger.info(s"Got locally generated modifier ${lm.pmod.encodedId} of type ${lm.pmod.modifierTypeId}")
      lm.pmod match {
        case block: Block =>
          pmodModify(block.header, isLocallyGenerated = true)
          pmodModify(block.payload, isLocallyGenerated = true)
        case anyMod =>
          pmodModify(anyMod, isLocallyGenerated = true)
      }
      logger.debug(s"Time processing of msg LocallyGeneratedModifier with mod of type ${lm.pmod.modifierTypeId}:" +
        s" with id: ${Algos.encode(lm.pmod.id)} -> ${System.currentTimeMillis() - startTime}")

    case GetDataFromCurrentView(f) =>
      f(CurrentView(nodeView.history, nodeView.state, nodeView.wallet)) match {
        case resultFuture: Future[_] => resultFuture.pipeTo(sender())
        case result => sender() ! result
      }

    case GetNodeViewChanges(history, state, _) =>
      if (history) sender() ! ChangedHistory(nodeView.history)
      if (state) sender() ! ChangedState(nodeView.state)

    case msg@CompareViews(peer, modifierTypeId, modifierIds) =>
      historyApplicator ! CompareViewsWithSender(peer, modifierTypeId, modifierIds, sender())
//      val ids: Seq[ModifierId] = modifierTypeId match {
//        case _ => modifierIds
//          .filterNot(mid => nodeView.history.isModifierDefined(mid) || ModifiersCache.contains(key(mid)))
//      }
//      if (modifierTypeId != Transaction.modifierTypeId) logger.debug(s"Got compare view message on NVH from ${peer.socketAddress}." +
//        s" Type of requesting modifiers is: $modifierTypeId. Requesting ids size are: ${ids.size}." +
//        s" Sending RequestFromLocal with ids to $sender." +
//        s"\n Requesting ids are: ${ids.map(Algos.encode).mkString(",")}.")
//      if (ids.nonEmpty && (modifierTypeId == Header.modifierTypeId || (nodeView.history.isHeadersChainSynced && modifierTypeId == Payload.modifierTypeId)))
//        sender() ! RequestFromLocal(peer, modifierTypeId, ids)
    case msg => logger.error(s"Got strange message on nvh: $msg")
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

  def updateNodeView(updatedHistory: Option[History] = None,
                     updatedState: Option[UtxoState] = None,
                     updatedVault: Option[EncryWallet] = None): Unit = {
    val newNodeView: NodeView = NodeView(updatedHistory.getOrElse(nodeView.history),
      updatedState.getOrElse(nodeView.state),
      updatedVault.getOrElse(nodeView.wallet))
    if (updatedHistory.nonEmpty) {

    }
    if (updatedState.nonEmpty) context.system.eventStream.publish(ChangedState(newNodeView.state))
    nodeView = newNodeView
  }

  def requestDownloads(pi: ProgressInfo, previousModifier: Option[ModifierId] = None): Unit =
    pi.toDownload.foreach { case (tid, id) =>
      if (tid != Transaction.modifierTypeId) logger.debug(s"NVH trigger sending DownloadRequest to NVSH with type: $tid " +
        s"for modifier: ${Algos.encode(id)}. PrevMod is: ${previousModifier.map(Algos.encode)}.")
      nodeViewSynchronizer ! DownloadRequest(tid, id, previousModifier)
    }

  def trimChainSuffix(suffix: IndexedSeq[PersistentModifier], rollbackPoint: ModifierId):
  IndexedSeq[PersistentModifier] = {
    val idx: Int = suffix.indexWhere(_.id.sameElements(rollbackPoint))
    if (idx == -1) IndexedSeq() else suffix.drop(idx)
  }

  private def updateState(history: History,
                          state: UtxoState,
                          progressInfo: ProgressInfo,
                          suffixApplied: IndexedSeq[PersistentModifier]):
  (History, UtxoState, Seq[PersistentModifier]) = {
//    logger.debug(s"\nStarting updating state in updateState function!")
//    progressInfo.toApply.foreach {
//      case header: Header => requestDownloads(progressInfo, Some(header.id))
//      case _ => requestDownloads(progressInfo, None)
//    }
    val branchingPointOpt: Option[VersionTag] = progressInfo.branchPoint.map(VersionTag !@@ _)
    val (stateToApplyTry: Try[UtxoState], suffixTrimmed: IndexedSeq[PersistentModifier]@unchecked) =
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
            case Right(stateAfterApply) =>
              influxRef.foreach(ref => modToApply match {
                case b: Block if history.isFullChainSynced => ref ! TransactionsInBlock(b.payload.txs.size)
                case _ =>
              })
              val newHis: History = history.reportModifierIsValid(modToApply)
              context.system.eventStream.publish(SemanticallySuccessfulModifier(modToApply))
              UpdateInformation(newHis, stateAfterApply, None, None, u.suffix :+ modToApply)
            case Left(e) =>
              val (newHis: History, newProgressInfo: ProgressInfo) =
                history.reportModifierIsInvalid(modToApply)
              context.system.eventStream.publish(SemanticallyFailedModification(modToApply, e))
              UpdateInformation(newHis, u.state, Some(modToApply), Some(newProgressInfo), u.suffix)
          } else u
        }
        uf.failedMod match {
          case Some(_) => updateState(uf.history, uf.state, uf.alternativeProgressInfo.get, uf.suffix)
          case None => (uf.history, uf.state, uf.suffix)
        }
      case Failure(e) =>
        context.system.eventStream.publish(RollbackFailed(branchingPointOpt))
        EncryApp.forceStopApplication(500, s"Rollback failed: $e")
    }
  }

  def pmodModify(pmod: PersistentModifier, isLocallyGenerated: Boolean = false): Unit =
    if (!nodeView.history.isModifierDefined(pmod.id)) {
      nodeView.history.append(pmod) match {
        case Right(progressInfo) =>
//          if (progressInfo.toApply.nonEmpty) {
//            val (newHistory: History, newState: UtxoState, blocksApplied: Seq[PersistentModifier]) =
//              updateState(historyBeforeStUpdate, nodeView.state, progressInfo, IndexedSeq())
//            sendUpdatedInfoTo MemoryPool(progressInfo.toRemove)
//            if (progressInfo.chainSwitchingNeeded)
//              nodeView.wallet.rollback(VersionTag !@@ progressInfo.branchPoint.get).get
//            blocksApplied.foreach(nodeView.wallet.scanPersistent)
//            if (newHistory.isFullChainSynced) {
//              logger.debug(s"\nblockchain is synced on nvh on height ${newHistory.getBestHeaderHeight}!")
//              ModifiersCache.setChainSynced()
//              Seq(nodeViewSynchronizer, miner).foreach(_ ! FullBlockChainIsSynced)
//            }
//            updateNodeView(Some(newHistory), Some(newState), Some(nodeView.wallet))
//          } else {
//            if (!isLocallyGenerated) requestDownloads(progressInfo, Some(pmod.id))
//            context.system.eventStream.publish(SemanticallySuccessfulModifier(pmod))
//            updateNodeView(updatedHistory = Some(historyBeforeStUpdate))
//          }
        case Left(e) =>
          context.system.eventStream.publish(SyntacticallyFailedModification(pmod, List(HistoryApplyError(e.getMessage))))
      }
    }
    else ()

  def sendUpdatedInfoToMemoryPool(toRemove: Seq[PersistentModifier]): Unit = {
    val rolledBackTxs: IndexedSeq[Transaction] = toRemove
      .flatMap(extractTransactions)
      .toIndexedSeq
    if (rolledBackTxs.nonEmpty)
      memoryPoolRef ! RolledBackTransactions(rolledBackTxs)
  }

  def extractTransactions(mod: PersistentModifier): Seq[Transaction] = mod match {
    case b: Block => b.payload.txs
    case p: Payload => p.txs
    case _ => Seq.empty[Transaction]
  }

  def genesisState: NodeView = {
    val stateDir: File = UtxoState.getStateDir(settings)
    stateDir.mkdir()
    assert(stateDir.listFiles().isEmpty, s"Genesis directory $stateDir should always be empty.")
    val state: UtxoState = UtxoState.genesis(stateDir, Some(self), settings, influxRef)
    val history: History = History.readOrGenerate(settings, timeProvider)
    val wallet: EncryWallet = EncryWallet.readOrGenerate(settings)
    NodeView(history, state, wallet)
  }

  def restoreState(): Option[NodeView] = if (History.getHistoryIndexDir(settings).listFiles.nonEmpty)
    try {
      val stateDir: File = UtxoState.getStateDir(settings)
      stateDir.mkdirs()
      val history: History = History.readOrGenerate(settings, timeProvider)
      val wallet: EncryWallet = EncryWallet.readOrGenerate(settings)
      val state: UtxoState = restoreConsistentState(
        UtxoState.create(stateDir, Some(self), settings, influxRef), history
      )
      Some(NodeView(history, state, wallet))
    } catch {
      case ex: Throwable =>
        logger.info(s"${ex.getMessage} during state restore. Recover from Modifiers holder!")
        new File(settings.directory).listFiles.foreach(dir => FileUtils.cleanDirectory(dir))
        Some(genesisState)
    } else {
    None
  }

  def getRecreatedState(version: Option[VersionTag] = None, digest: Option[ADDigest] = None): UtxoState = {
    val dir: File = UtxoState.getStateDir(settings)
    dir.mkdirs()
    dir.listFiles.foreach(_.delete())
    val stateDir: File = UtxoState.getStateDir(settings)
    stateDir.mkdirs()
    UtxoState.create(stateDir, Some(self), settings, influxRef)
  }

  def restoreConsistentState(stateIn: UtxoState, history: History): UtxoState =
    (stateIn.version, history.getBestBlock, stateIn) match {
      case (stateId, None, _) if stateId sameElements Array.emptyByteArray =>
        logger.info(s"State and history are both empty on startup")
        stateIn
      case (stateId, Some(block), _) if stateId sameElements block.id =>
        logger.info(s"State and history have the same version ${Algos.encode(stateId)}, no recovery needed.")
        stateIn
      case (_, None, _) =>
        logger.info(s"State and history are inconsistent." +
          s" History is empty on startup, rollback state to genesis.")
        getRecreatedState()
      case (stateId, Some(historyBestBlock), state: UtxoState) =>
        val stateBestHeaderOpt = history.getHeaderById(ModifierId !@@ stateId) //todo naming
        val (rollbackId, newChain) = history.getChainToHeader(stateBestHeaderOpt, historyBestBlock.header)
        logger.info(s"State and history are inconsistent." +
          s" Going to rollback to ${rollbackId.map(Algos.encode)} and " +
          s"apply ${newChain.length} modifiers")
        val startState = rollbackId.map(id => state.rollbackTo(VersionTag !@@ id).get)
          .getOrElse(getRecreatedState())
        val toApply = newChain.headers.map { h =>
          history.getBlockByHeader(h) match {
            case Some(fb) => fb
            case None => throw new Exception(s"Failed to get full block for header $h")
          }
        }
        toApply.foldLeft(startState) { (s, m) => s.applyModifier(m).right.get }
    }

  override def close(): Unit = {
    nodeView.history.close()
    nodeView.state.close()
    nodeView.wallet.close()
  }
}

object NodeViewHolder {

  final case class DownloadRequest(modifierTypeId: ModifierTypeId,
                                   modifierId: ModifierId,
                                   previousModifier: Option[ModifierId] = None) extends NodeViewHolderEvent

  case class CurrentView[HIS, MS, VL](history: HIS, state: MS, vault: VL)

  case class UpdateInformation(history: History,
                               state: UtxoState,
                               failedMod: Option[PersistentModifier],
                               alternativeProgressInfo: Option[ProgressInfo],
                               suffix: IndexedSeq[PersistentModifier])

  object ReceivableMessages {

    case class GetNodeViewChanges(history: Boolean, state: Boolean, vault: Boolean)

    case class GetDataFromCurrentView[HIS, MS, VL, A](f: CurrentView[HIS, MS, VL] => A)

    case object GetWallet

    case class CompareViews(source: ConnectedPeer, modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId])

    case class CompareViewsWithSender(source: ConnectedPeer,
                                      modifierTypeId: ModifierTypeId,
                                      modifierIds: Seq[ModifierId],
                                      sender: ActorRef)

    final case class ModifierFromRemote(serializedModifiers: PersistentModifier) extends AnyVal

    case class LocallyGeneratedModifier(pmod: PersistentModifier)

  }

  class NodeViewHolderPriorityQueue(settings: ActorSystem.Settings, config: Config)
    extends UnboundedStablePriorityMailbox(
      PriorityGenerator {
        case CompareViews(_, _, _) => 0

        case PoisonPill => 2

        case otherwise => 1
      })

  def props(memoryPoolRef: ActorRef, influxRef: Option[ActorRef], dataHolder: ActorRef): Props =
    Props(new NodeViewHolder(memoryPoolRef, influxRef, dataHolder))
}