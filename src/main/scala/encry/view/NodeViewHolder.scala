package encry.view

import java.io.File
import HeaderProto.HeaderProtoMessage
import PayloadProto.PayloadProtoMessage
import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props}
import akka.dispatch.{PriorityGenerator, UnboundedStablePriorityMailbox}
import akka.pattern._
import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import encry.EncryApp
import encry.EncryApp._
import encry.consensus.History.ProgressInfo
import encry.network.BlackList.SyntacticallyInvalidModifier
import encry.modifiers.history.{PayloadUtils => PU, HeaderUtils => HU}
import encry.network.DeliveryManager.FullBlockChainIsSynced
import encry.network.NodeViewSynchronizer.ReceivableMessages._
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.network.PeersKeeper.BanPeer
import encry.network.PrioritiesCalculator.PeersPriorityStatus.PeersPriorityStatus
import encry.stats.StatsSender._
import encry.utils.CoreTaggedTypes.VersionTag
import encry.view.NodeViewHolder._
import encry.view.NodeViewHolder.ReceivableMessages._
import encry.view.history.EncryHistory
import encry.view.mempool.Mempool._
import encry.view.state._
import encry.view.wallet.EncryWallet
import org.apache.commons.io.FileUtils
import org.encryfoundation.common.modifiers.{NodeViewModifier, PersistentModifier}
import org.encryfoundation.common.modifiers.history._
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.serialization.Serializer
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ADDigest, ModifierId, ModifierTypeId}
import scala.annotation.tailrec
import scala.collection.{mutable, IndexedSeq, Seq}
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

class NodeViewHolder[StateType <: EncryState[StateType]](memoryPoolRef: ActorRef,
                                                         influxRef: Option[ActorRef]) extends Actor with StrictLogging {
  case class NodeView(history: EncryHistory, state: StateType, wallet: EncryWallet)

  var applicationsSuccessful: Boolean = true
  var nodeView: NodeView = restoreState().getOrElse(genesisState)

//  val auxHistoryHolder: ActorRef =
//    system.actorOf(Props(AuxiliaryHistoryHolder(nodeView.history, nodeViewSynchronizer))
//      .withDispatcher("aux-history-dispatcher"), "auxHistoryHolder")

  memoryPoolRef ! UpdatedState(nodeView.state)

  influxRef.foreach(ref => context.system.scheduler.schedule(5.second, 5.second) {
    ref ! HeightStatistics(nodeView.history.bestHeaderHeight, nodeView.history.bestBlockHeight)
  })

  val modifierSerializers: Map[ModifierTypeId, Serializer[_ <: NodeViewModifier]] = Map(
    Header.modifierTypeId   -> HeaderSerializer,
    Payload.modifierTypeId  -> PayloadSerializer,
    ADProofs.modifierTypeId -> ADProofSerializer
  )

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    reason.printStackTrace()
    System.exit(100)
  }

  context.system.scheduler.schedule(5.seconds, 10.seconds)(logger.debug(s"Modifiers cache from NVH: " +
    s"${ModifiersCache.size}. Elems: ${ModifiersCache.cache.keys.map(key => Algos.encode(key.toArray)).mkString(",")}"))

  override def postStop(): Unit = {
    logger.warn(s"Stopping NodeViewHolder...")
    nodeView.history.closeStorage()
    nodeView.state.closeStorage()
  }

  override def receive: Receive = {
    case ModifiersFromRemote(modifierTypeId, modifiers, peer) => modifierTypeId match {
      case Payload.modifierTypeId => modifiers.foreach(serializedModifier =>
        PayloadProtoSerializer.fromProto(PayloadProtoMessage.parseFrom(serializedModifier)) match {
          case Success(payload) if PU.semanticValidity(payload).isSuccess && PU.syntacticallyValidity(payload).isSuccess =>
            logger.info(s"Successfully serialized payload with id: ${Algos.encode(payload.id)}.")
            val isInHistory: Boolean = nodeView.history.contains(payload.id)
            val isInCache: Boolean = ModifiersCache.contains(key(payload.id))
            if (isInHistory || isInCache)
              logger.info(s"Received payload ${Algos.encode(payload.id)} can't be placed into cache cause of: " +
                s"inHistory: $isInHistory, inCache: $isInCache.")
            else ModifiersCache.put(key(payload.id), payload, nodeView.history)
          case Success(_) => peersKeeper ! BanPeer(peer, SyntacticallyInvalidModifier)
          case Failure(ex) =>
            peersKeeper ! BanPeer(peer, SyntacticallyInvalidModifier)
            logger.debug(s"Received modifier from $peer can't be parsed cause of: ${ex.getMessage}.")
        })
        computeApplications()
      case Header.modifierTypeId => modifiers.foreach(serializedModifier =>
        HeaderProtoSerializer.fromProto(HeaderProtoMessage.parseFrom(serializedModifier)) match {
          case Success(header) if HU.semanticValidity(header).isSuccess && HU.syntacticallyValidity(header).isSuccess =>
            logger.info(s"Successfully serialized header with id: ${Algos.encode(header.id)}.")
            val isInHistory: Boolean = nodeView.history.contains(header.id)
            val isInCache: Boolean = ModifiersCache.contains(key(header.id))
            if (isInHistory || isInCache)
              logger.info(s"Received header ${Algos.encode(header.id)} can't be placed into cache cause of: " +
                s"inHistory: $isInHistory, inCache: $isInCache.")
            else ModifiersCache.put(key(header.id), header, nodeView.history)
          case Success(_) => peersKeeper ! BanPeer(peer, SyntacticallyInvalidModifier)
          case Failure(ex) =>
            peersKeeper ! BanPeer(peer, SyntacticallyInvalidModifier)
            logger.debug(s"Received modifier from $peer can't be parsed cause of: ${ex.getMessage}.")
        })
        computeApplications()
      case modType => logger.info(s"NodeViewHolder got modifier of wrong type: $modType!")
    }

    case lm: LocallyGeneratedModifier[PersistentModifier]@unchecked =>
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

    case CompareViews(peer, modifierTypeId, modifierIds) =>
      logger.debug(s"Start processing CompareViews message on NVH.")
      val startTime = System.currentTimeMillis()
      val ids: Seq[ModifierId] = modifierTypeId match {
        case _ => modifierIds.filterNot(mid => nodeView.history.contains(mid) || ModifiersCache.contains(key(mid)))
      }
      if (modifierTypeId != Transaction.modifierTypeId) logger.debug(s"Got compare view message on NVH from ${peer.socketAddress}." +
        s" Type of requesting modifiers is: $modifierTypeId. Requesting ids size are: ${ids.size}." +
        s" Sending RequestFromLocal with ids to $sender." +
        s"\n Requesting ids are: ${ids.map(Algos.encode).mkString(",")}.")
      if (ids.nonEmpty) peersKeeper ! GetPeersForRequestFromLocal(peer, modifierTypeId, ids)
      logger.debug(s"Time processing of msg CompareViews from $sender with modTypeId $modifierTypeId: ${System.currentTimeMillis() - startTime}")

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

  def updateNodeView(updatedHistory: Option[EncryHistory] = None,
                     updatedState: Option[StateType] = None,
                     updatedVault: Option[EncryWallet] = None): Unit = {
    val newNodeView: NodeView = NodeView(updatedHistory.getOrElse(nodeView.history),
      updatedState.getOrElse(nodeView.state),
      updatedVault.getOrElse(nodeView.wallet))
    if (updatedHistory.nonEmpty) {
      nodeViewSynchronizer ! ChangedHistory(newNodeView.history)
      context.system.eventStream.publish(ChangedHistory(newNodeView.history))
    }
    if (updatedState.nonEmpty) context.system.eventStream.publish(ChangedState(newNodeView.state))
    nodeView = newNodeView
  }

  def requestDownloads(pi: ProgressInfo[PersistentModifier], previousModifier: Option[ModifierId] = None): Unit =
    pi.toDownload.foreach { case (tid, id) =>
      if (tid != Transaction.modifierTypeId) logger.debug(s"NVH trigger sending DownloadRequest to NVSH with type: $tid " +
        s"for modifier: ${Algos.encode(id)}. PrevMod is: ${previousModifier.map(Algos.encode)}.")

      peersKeeper ! PrepareForDownloadRequest(tid, id, previousModifier)
    }

  def trimChainSuffix(suffix: IndexedSeq[PersistentModifier], rollbackPoint: ModifierId):
  IndexedSeq[PersistentModifier] = {
    val idx: Int = suffix.indexWhere(_.id.sameElements(rollbackPoint))
    if (idx == -1) IndexedSeq() else suffix.drop(idx)
  }

  @tailrec
  private def updateState(history: EncryHistory,
                          state: StateType,
                          progressInfo: ProgressInfo[PersistentModifier],
                          suffixApplied: IndexedSeq[PersistentModifier]):
  (EncryHistory, Try[StateType], Seq[PersistentModifier]) = {
    case class UpdateInformation(history: EncryHistory,
                                 state: StateType,
                                 failedMod: Option[PersistentModifier],
                                 alternativeProgressInfo: Option[ProgressInfo[PersistentModifier]],
                                 suffix: IndexedSeq[PersistentModifier])
    logger.debug(s"\nStarting updating state in updateState function!")
    val startTime = System.currentTimeMillis()
    requestDownloads(progressInfo, None)
    val branchingPointOpt: Option[VersionTag] = progressInfo.branchPoint.map(VersionTag !@@ _)
    val (stateToApplyTry: Try[StateType], suffixTrimmed: IndexedSeq[PersistentModifier]@unchecked) =
      if (progressInfo.chainSwitchingNeeded) {
        branchingPointOpt.map { branchPoint =>
          if (!state.version.sameElements(branchPoint))
            state.rollbackTo(branchPoint) -> trimChainSuffix(suffixApplied, ModifierId !@@ branchPoint)
          else Success(state) -> IndexedSeq()
        }.getOrElse(Failure(new Exception("Trying to rollback when branchPoint is empty.")))
      } else Success(state) -> suffixApplied
    stateToApplyTry match {
      case Success(stateToApply) =>
        logger.debug(s"\nApplied to state successfully! Time of it is: ${System.currentTimeMillis() - startTime}.")
        logger.debug(s"\nStarting to update UpdateInformation !")
        val startTime1 = System.currentTimeMillis()
        context.system.eventStream.publish(RollbackSucceed(branchingPointOpt))
        val u0: UpdateInformation = UpdateInformation(history, stateToApply, None, None, suffixTrimmed)
        val uf: UpdateInformation = progressInfo.toApply.foldLeft(u0) { case (u, modToApply) =>
          //here
          if (u.failedMod.isEmpty) u.state.applyModifier(modToApply) match {
            case Success(stateAfterApply) =>
              modToApply match {
                case block: Block if settings.influxDB.isDefined =>
                  context.system.actorSelection("user/statsSender") ! TxsInBlock(block.payload.txs.size)
                case _ =>
              }
              val newHis: EncryHistory = history.reportModifierIsValid(modToApply)
              //auxHistoryHolder ! NewHistory(newHis)
              context.system.eventStream.publish(SemanticallySuccessfulModifier(modToApply))
              UpdateInformation(newHis, stateAfterApply, None, None, u.suffix :+ modToApply)
            case Failure(e) =>
              val (newHis: EncryHistory, newProgressInfo: ProgressInfo[PersistentModifier]) =
                history.reportModifierIsInvalid(modToApply, progressInfo)
              //auxHistoryHolder ! NewHistory(newHis)
              context.system.eventStream.publish(SemanticallyFailedModification(modToApply, e))
              UpdateInformation(newHis, u.state, Some(modToApply), Some(newProgressInfo), u.suffix)
          } else u
        }
        logger.debug(s"\n\nFinished UpdateInformation! Time is: ${System.currentTimeMillis() - startTime1}")
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

  def pmodModify(pmod: PersistentModifier, isLocallyGenerated: Boolean = false): Unit = if (!nodeView.history.contains(pmod.id)) {
    logger.debug(s"\nStarting to apply modifier ${pmod.encodedId} of type ${pmod.modifierTypeId} on nodeViewHolder to history.")
    val startAppHistory = System.currentTimeMillis()
    if (settings.influxDB.isDefined) context.system
      .actorSelection("user/statsSender") !
      StartApplyingModif(pmod.id, pmod.modifierTypeId, System.currentTimeMillis())
    nodeView.history.append(pmod) match {
      case Success((historyBeforeStUpdate, progressInfo)) =>
        logger.debug(s"Successfully applied modifier ${pmod.encodedId} of type ${pmod.modifierTypeId} on nodeViewHolder to history.")
        logger.debug(s"Time of applying to history SUCCESS is: ${System.currentTimeMillis() - startAppHistory}. modId is: ${pmod.encodedId}")
        influxRef.foreach { ref =>
          ref ! EndOfApplyingModif(pmod.id)
          val isHeader: Boolean = pmod match {
            case _: Header => true
            case _: Payload => false
          }
          ref ! ModifierAppendedToHistory(isHeader, success = true)
        }
        logger.info(s"Going to apply modifications ${pmod.encodedId} of type ${pmod.modifierTypeId} on nodeViewHolder to the state: $progressInfo")
        val startAppState = System.currentTimeMillis()
        if (progressInfo.toApply.nonEmpty) {
          logger.debug(s"\n progress info non empty")
          val startPoint: Long = System.currentTimeMillis()
          val (newHistory: EncryHistory, newStateTry: Try[StateType], blocksApplied: Seq[PersistentModifier]) =
            updateState(historyBeforeStUpdate, nodeView.state, progressInfo, IndexedSeq())
          if (settings.influxDB.isDefined)
            context.actorSelection("/user/statsSender") ! StateUpdating(System.currentTimeMillis() - startPoint)
          newStateTry match {
            case Success(newMinState) =>
              logger.debug(s"\nTime of applying to state SUCCESS is: ${System.currentTimeMillis() - startAppState}. modId is: ${pmod.encodedId}")
              logger.debug(s"\nStarting inner updating after succeeded state applying!")
              influxRef.foreach { ref =>
                val isBlock: Boolean = pmod match {
                  case _: Payload => true
                  case _ => false
                }
                if (isBlock) ref ! ModifierAppendedToState(success = true)
              }
              val startInnerStateApp = System.currentTimeMillis()
              sendUpdatedInfoToMemoryPool(newMinState, progressInfo.toRemove, blocksApplied)
              if (progressInfo.chainSwitchingNeeded)
                nodeView.wallet.rollback(VersionTag !@@ progressInfo.branchPoint.get).get
              blocksApplied.foreach(nodeView.wallet.scanPersistent)
              logger.debug(s"\nPersistent modifier ${pmod.encodedId} applied successfully")
              if (settings.influxDB.isDefined) newHistory.bestHeaderOpt.foreach(header =>
                context.actorSelection("/user/statsSender") ! BestHeaderInChain(header, System.currentTimeMillis()))
              if (newHistory.isFullChainSynced) {
                logger.debug(s"\nblockchain is synced on nvh on height ${newHistory.bestHeaderHeight}!")
                ModifiersCache.setChainSynced()
                Seq(nodeViewSynchronizer, miner).foreach(_ ! FullBlockChainIsSynced)
              }
              updateNodeView(Some(newHistory), Some(newMinState), Some(nodeView.wallet))
              logger.debug(s"\nTime of applying in inner state is: ${System.currentTimeMillis() - startInnerStateApp}")
            case Failure(e) =>
              logger.debug(s"\nCan`t apply persistent modifier (id: ${pmod.encodedId}, contents: $pmod) " +
                s"to minimal state because of: $e")
              influxRef.foreach { ref =>
                val isBlock: Boolean = pmod match {
                  case _: Payload => true
                  case _ => false
                }
                if (isBlock) ref ! ModifierAppendedToState(success = true)
              }
              logger.debug(s"\nTime of applying to state FAILURE is: ${System.currentTimeMillis() - startAppState}. modId is: ${pmod.encodedId}")
              updateNodeView(updatedHistory = Some(newHistory))
              context.system.eventStream.publish(SemanticallyFailedModification(pmod, e))
          }
        } else {
          if (!isLocallyGenerated) requestDownloads(progressInfo, Some(pmod.id))
          context.system.eventStream.publish(SemanticallySuccessfulModifier(pmod))
          logger.debug(s"\nProgres info is empty")
          updateNodeView(updatedHistory = Some(historyBeforeStUpdate))
        }
      case Failure(e) =>
        logger.debug(s"\nTime of applying to history FAILURE is: ${System.currentTimeMillis() - startAppHistory}. modId is: ${pmod.encodedId}")
        influxRef.foreach { ref =>
          val isHeader: Boolean = pmod match {
            case _: Header => true
            case _: Payload => false
          }
          ref ! ModifierAppendedToHistory(isHeader, success = false)
        }
        logger.debug(s"\nCan`t apply persistent modifier (id: ${pmod.encodedId}, contents: $pmod)" +
          s" to history caused $e")
        context.system.eventStream.publish(SyntacticallyFailedModification(pmod, e))
    }
  } else logger.info(s"\nTrying to apply modifier ${pmod.encodedId} that's already in history.")

  def sendUpdatedInfoToMemoryPool(state: StateType,
                                  toRemove: Seq[PersistentModifier],
                                  blocksApplied: Seq[PersistentModifier]): Unit = {
    memoryPoolRef ! UpdatedState(state)
    val rolledBackTxs: IndexedSeq[Transaction] =
      toRemove.flatMap(extractTransactions).toIndexedSeq
    if (rolledBackTxs.nonEmpty)
      memoryPoolRef ! RolledBackTransactions(rolledBackTxs)
    val appliedTransactions: IndexedSeq[Transaction] = blocksApplied.flatMap(extractTransactions).toIndexedSeq
    if (appliedTransactions.nonEmpty)
      memoryPoolRef ! TransactionsForRemove(appliedTransactions)
  }

  def extractTransactions(mod: PersistentModifier): Seq[Transaction] = mod match {
    case b: Block => b.payload.txs
    case p: Payload => p.txs
    case _ => Seq.empty[Transaction]
  }

  def genesisState: NodeView = {
    val stateDir: File = EncryState.getStateDir(settings)
    stateDir.mkdir()
    assert(stateDir.listFiles().isEmpty, s"Genesis directory $stateDir should always be empty.")
    val state: StateType = {
      if (settings.node.stateMode.isDigest) EncryState.generateGenesisDigestState(stateDir, settings)
      else EncryState.generateGenesisUtxoState(stateDir, Some(self), settings, influxRef)
    }.asInstanceOf[StateType]
    val history: EncryHistory = EncryHistory.readOrGenerate(settings, timeProvider)
    val wallet: EncryWallet = EncryWallet.readOrGenerate(settings)
    NodeView(history, state, wallet)
  }

  def restoreState(): Option[NodeView] =
    if (EncryHistory.getHistoryIndexDir(settings).listFiles.nonEmpty)
      try {
        val history: EncryHistory = EncryHistory.readOrGenerate(settings, timeProvider)
        val wallet: EncryWallet = EncryWallet.readOrGenerate(settings)
        val state: StateType =
          restoreConsistentState(EncryState.readOrGenerate(settings, Some(self), influxRef).asInstanceOf[StateType], history)
        Some(NodeView(history, state, wallet))
      } catch {
        case ex: Throwable =>
          logger.info(s"${ex.getMessage} during state restore. Recover from Modifiers holder!")
          new File(settings.directory).listFiles.foreach(dir => FileUtils.cleanDirectory(dir))
          Some(genesisState)
      } else {
      logger.info("none")
      None
    }

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

object NodeViewHolder {

  case class DownloadRequest(modifierTypeId: ModifierTypeId,
                             modifierId: ModifierId,
                             previousModifier: Option[ModifierId] = None,
                             peers: IndexedSeq[(ConnectedPeer, PeersPriorityStatus)]) extends NodeViewHolderEvent

  final case class PrepareForDownloadRequest(modifierTypeId: ModifierTypeId,
                                             modifierId: ModifierId,
                                             previousModifier: Option[ModifierId] = None)

  case class CurrentView[HIS, MS, VL](history: HIS, state: MS, vault: VL)

  object ReceivableMessages {

    case class GetNodeViewChanges(history: Boolean, state: Boolean, vault: Boolean)

    case class GetDataFromCurrentView[HIS, MS, VL, A](f: CurrentView[HIS, MS, VL] => A)

    case object GetWallet

    case class CompareViews(source: ConnectedPeer, modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId])

    case class ModifiersFromRemote(modTypeId: ModifierTypeId,
                                   serializedModifiers: Seq[Array[Byte]],
                                   modifiersSender: ConnectedPeer)

    case class LocallyGeneratedTransaction(tx: Transaction)

    case class LocallyGeneratedModifier[EncryPersistentModifier <: PersistentModifier](pmod: EncryPersistentModifier)

  }

  class EncryNodeViewHolderPriorityQueue(settings: ActorSystem.Settings, config: Config)
    extends UnboundedStablePriorityMailbox(
      PriorityGenerator {
        case CompareViews(_, _, _) => 0

        case PoisonPill => 2

        case otherwise => 1
      })

  def props(memoryPoolRef: ActorRef, peersKeeper: ActorRef, influxRef: Option[ActorRef]): Props =
    settings.node.stateMode match {
      case StateMode.Digest => Props(new NodeViewHolder[DigestState](memoryPoolRef, peersKeeper, influxRef))
      case StateMode.Utxo => Props(new NodeViewHolder[UtxoState](memoryPoolRef, peersKeeper, influxRef))
    }
}