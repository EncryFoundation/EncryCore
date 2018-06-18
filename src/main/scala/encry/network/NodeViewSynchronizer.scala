package encry.network

import java.net.InetSocketAddress

import akka.actor.Actor
import encry.EncryApp._
import encry.consensus.{HistoryReader, SyncInfo}
import encry.network.PeerConnectionHandler._
import encry.network.message.BasicMsgDataTypes._
import encry.network.message.{InvSpec, RequestModifierSpec, _}
import encry.settings.NetworkSettings
import encry.utils.ScorexLogging
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.StateReader
import scorex.core.transaction.{MempoolReader, Transaction}
import scorex.core.{PersistentNodeViewModifier, _}
import scorex.crypto.encode.Base58

import scala.concurrent.duration._
import scala.language.postfixOps

/**
  * A component which synchronizes local node view with p2p network.
  *
  * @param syncInfoSpec SyncInfo specification
  * @tparam P   proposition
  * @tparam TX  transaction
  * @tparam SIS SyncInfoMessage specification
  */
class NodeViewSynchronizer[P <: Proposition, TX <: Transaction[P], SI <: SyncInfo,
SIS <: SyncInfoMessageSpec[SI], PMOD <: PersistentNodeViewModifier, HR <: HistoryReader[PMOD, SI], MR <: MempoolReader[TX]]
(syncInfoSpec: SIS) extends Actor with ScorexLogging {

  import NodeViewSynchronizer.ReceivableMessages._
  import encry.consensus.History._
  import encry.network.NetworkController.ReceivableMessages.{DataFromPeer, RegisterMessagesHandler, SendToNetwork}
  import encry.view.EncryNodeViewHolder.ReceivableMessages.{CompareViews, GetNodeViewChanges, ModifiersFromRemote}

  val networkSettings: NetworkSettings = settings.network

  val deliveryTimeout: FiniteDuration = networkSettings.deliveryTimeout
  val maxDeliveryChecks: Int = networkSettings.maxDeliveryChecks
  val invSpec: InvSpec = new InvSpec(networkSettings.maxInvObjects)
  val requestModifierSpec = new RequestModifierSpec(networkSettings.maxInvObjects)

  val deliveryTracker: DeliveryTracker = new DeliveryTracker(context, deliveryTimeout, maxDeliveryChecks, self)
  val statusTracker: SyncTracker = SyncTracker(self, context, networkSettings, timeProvider)

  var historyReaderOpt: Option[HR] = None
  var mempoolReaderOpt: Option[MR] = None

  def readersOpt: Option[(HR, MR)] = historyReaderOpt.flatMap(h => mempoolReaderOpt.map(mp => (h, mp)))

  def broadcastModifierInv[M <: NodeViewModifier](m: M): Unit =
    networkController ! SendToNetwork(Message(invSpec, Right(m.modifierTypeId -> Seq(m.id)), None), Broadcast)

  @SuppressWarnings(Array("org.wartremover.warts.IsInstanceOf"))
  def viewHolderEvents: Receive = {
    case SuccessfulTransaction(tx) => broadcastModifierInv(tx)
    case SyntacticallySuccessfulModifier(mod) =>
    case SyntacticallyFailedModification(mod, throwable) =>
    case SemanticallySuccessfulModifier(mod) => broadcastModifierInv(mod)
    case SemanticallyFailedModification(mod, throwable) =>
    case ChangedHistory(reader: HR@unchecked) if reader.isInstanceOf[HR] => historyReaderOpt = Some(reader)
    case ChangedMempool(reader: MR@unchecked) if reader.isInstanceOf[MR] => mempoolReaderOpt = Some(reader)
  }

  def peerManagerEvents: Receive = {
    case HandshakedPeer(remote) => statusTracker.updateStatus(remote, Unknown)
    case DisconnectedPeer(remote) => statusTracker.clearStatus(remote)
  }

  def getLocalSyncInfo: Receive = {
    case SendLocalSyncInfo =>
      //todo: why this condition?
      if (statusTracker.elapsedTimeSinceLastSync() < (networkSettings.syncInterval.toMillis / 2))
      //TODO should never reach this point
        log.debug("Trying to send sync info too often")
      else historyReaderOpt.foreach(r => sendSync(r.syncInfo))
  }

  def sendSync(syncInfo: SI): Unit = {
    val peers = statusTracker.peersToSyncWith()
    if (peers.nonEmpty)
      networkController ! SendToNetwork(Message(syncInfoSpec, Right(syncInfo), None), SendToPeers(peers))
  }

  //sync info is coming from another node
  def processSync: Receive = {
    case DataFromPeer(spec, syncInfo: SI@unchecked, remote)
      if spec.messageCode == syncInfoSpec.messageCode =>
      historyReaderOpt match {
        case Some(historyReader) =>
          val extensionOpt: Option[ModifierIds] = historyReader.continuationIds(syncInfo, networkSettings.networkChunkSize)
          val ext: ModifierIds = extensionOpt.getOrElse(Seq())
          val comparison: HistoryComparisonResult = historyReader.compare(syncInfo)
          log.debug(s"Comparison with $remote having starting points ${idsToString(syncInfo.startingPoints)}. " +
            s"Comparison result is $comparison. Sending extension of length ${ext.length}")
          log.trace(s"Extension ids: ${idsToString(ext)}")
          if (!(extensionOpt.nonEmpty || comparison != Younger)) log.warn("Extension is empty while comparison is younger")
          self ! OtherNodeSyncingStatus(remote, comparison, extensionOpt)
        case _ =>
      }
  }

  // Send history extension to the (less developed) peer 'remote' which does not have it.
  def sendExtension(remote: ConnectedPeer,
                    status: HistoryComparisonResult,
                    extOpt: Option[Seq[(ModifierTypeId, ModifierId)]]): Unit = extOpt match {
    case None => log.warn(s"extOpt is empty for: $remote. Its status is: $status.")
    case Some(ext) => ext.groupBy(_._1).mapValues(_.map(_._2)).foreach {
      case (mid, mods) => networkController ! SendToNetwork(Message(invSpec, Right(mid -> mods), None), SendToPeer(remote))
    }
  }

  //view holder is telling other node status
  def processSyncStatus: Receive = {
    case OtherNodeSyncingStatus(remote, status, extOpt) =>
      statusTracker.updateStatus(remote, status)
      status match {
        case Unknown => log.warn("Peer status is still unknown")
        case Nonsense => log.warn("Got nonsense") //todo: fix, see https://github.com/ScorexFoundation/Scorex/issues/158
        case Younger => sendExtension(remote, status, extOpt)
        case _ => // does nothing for `Equal` and `Older`
      }
  }

  //object ids coming from other node
  def processInv: Receive = {
    case DataFromPeer(spec, invData: InvData@unchecked, remote)
      if spec.messageCode == InvSpec.MessageCode =>
      //TODO can't replace viewHolderRef with a reader because of modifiers cache
      nodeViewHolder ! CompareViews(remote, invData._1, invData._2)
  }

  //other node asking for objects by their ids
  def modifiersReq: Receive = {
    case DataFromPeer(spec, invData: InvData@unchecked, remote)
      if spec.messageCode == RequestModifierSpec.MessageCode =>
      readersOpt.foreach { readers =>
        val objs: Seq[NodeViewModifier] = invData._1 match {
          case typeId: ModifierTypeId if typeId == Transaction.ModifierTypeId =>
            readers._2.getAll(invData._2)
          case _: ModifierTypeId =>
            invData._2.flatMap(id => readers._1.modifierById(id))
        }
        log.debug(s"Requested ${invData._2.length} modifiers ${idsToString(invData)}, " +
          s"sending ${objs.length} modifiers ${idsToString(invData._1, objs.map(_.id))} ")
        self ! ResponseFromLocal(remote, invData._1, objs)
      }
  }

  /**
    * Logic to process modifiers got from another peer
    */
  def modifiersFromRemote: Receive = {
    case DataFromPeer(spec, data: ModifiersData@unchecked, remote) if spec.messageCode == ModifiersSpec.messageCode =>
      val typeId: ModifierTypeId = data._1
      val modifiers: Map[ModifierId, Array[Byte]] = data._2
      log.info(s"Got modifiers of type $typeId from remote connected peer: $remote")
      log.trace(s"Received modifier ids ${data._2.keySet.map(Base58.encode).mkString(",")}")
      for ((id, _) <- modifiers) deliveryTracker.receive(typeId, id, remote)
      val (spam: Map[ModifierId, Array[Byte]], fm: Map[ModifierId, Array[Byte]]) =
        modifiers partition { case (id, _) => deliveryTracker.isSpam(id) }
      if (spam.nonEmpty) {
        log.info(s"Spam attempt: peer $remote has sent a non-requested modifiers of type $typeId with ids" +
          s": ${spam.keys.map(Base58.encode)}")
        val mids: Seq[ModifierId] = spam.keys.toSeq
        deliveryTracker.deleteSpam(mids)
      }
      if (fm.nonEmpty) {
        val mods: Seq[Array[Byte]] = fm.values.toSeq
        nodeViewHolder ! ModifiersFromRemote(remote, typeId, mods)
      }
  }

  //local node sending object ids to remote
  def requestFromLocal: Receive = {
    case RequestFromLocal(peer, modifierTypeId, modifierIds) =>
      if (modifierIds.nonEmpty) {
        val msg: Message[(ModifierTypeId, Seq[ModifierId])] =
          Message(requestModifierSpec, Right(modifierTypeId -> modifierIds), None)
        peer.handlerRef ! msg
      }
      deliveryTracker.expect(peer, modifierTypeId, modifierIds)
  }

  //scheduler asking node view synchronizer to check whether requested messages have been delivered
  @SuppressWarnings(Array("org.wartremover.warts.JavaSerializable"))
  def checkDelivery: Receive = {
    case CheckDelivery(peer, modifierTypeId, modifierId) =>
      if (deliveryTracker.peerWhoDelivered(modifierId).contains(peer)) deliveryTracker.delete(modifierId)
      else {
        log.info(s"Peer $peer has not delivered asked modifier ${Base58.encode(modifierId)} on time")
        deliveryTracker.reexpect(peer, modifierTypeId, modifierId)
      }
  }

  //local node sending out objects requested to remote
  def responseFromLocal: Receive = {
    case ResponseFromLocal(peer, _, modifiers: Seq[NodeViewModifier]) =>
      if (modifiers.nonEmpty) {
        @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
        val modType: ModifierTypeId = modifiers.head.modifierTypeId
        val m: (ModifierTypeId, Map[ModifierId, Array[Byte]]) = modType -> modifiers.map(m => m.id -> m.bytes).toMap
        val msg: Message[(ModifierTypeId, Map[ModifierId, Array[Byte]])] = Message(ModifiersSpec, Right(m), None)
        peer.handlerRef ! msg
      }
  }

  override def preStart(): Unit = {
    //register as a handler for synchronization-specific types of messages
    val messageSpecs: Seq[MessageSpec[_]] = Seq(invSpec, requestModifierSpec, ModifiersSpec, syncInfoSpec)
    networkController ! RegisterMessagesHandler(messageSpecs, self)
    //register as a listener for peers got connected (handshaked) or disconnected
    context.system.eventStream.subscribe(self, classOf[DisconnectedPeer])
    // todo: replace the two lines above by a single line with classOf[PeerManagementEvent]


    //subscribe for all the node view holder events involving modifiers and transactions
    context.system.eventStream.subscribe(self, classOf[NodeViewChange])
    context.system.eventStream.subscribe(self, classOf[ModificationOutcome])
    nodeViewHolder ! GetNodeViewChanges(history = true, state = false, vault = false, mempool = true)

    statusTracker.scheduleSendSyncInfo()
  }

  override def receive: Receive =
    getLocalSyncInfo orElse
      processSync orElse
      processSyncStatus orElse
      processInv orElse
      modifiersReq orElse
      requestFromLocal orElse
      responseFromLocal orElse
      modifiersFromRemote orElse
      viewHolderEvents orElse
      peerManagerEvents orElse
      checkDelivery orElse {
      case a: Any => log.error("Strange input: " + a)
    }
}

object NodeViewSynchronizer {

  object Events {

    trait NodeViewSynchronizerEvent

    case object NoBetterNeighbour extends NodeViewSynchronizerEvent

    case object BetterNeighbourAppeared extends NodeViewSynchronizerEvent

  }

  object ReceivableMessages {

    case object SendLocalSyncInfo

    case class RequestFromLocal(source: ConnectedPeer, modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId])

    case class ResponseFromLocal[M <: NodeViewModifier](source: ConnectedPeer, modifierTypeId: ModifierTypeId, localObjects: Seq[M])

    case class CheckDelivery(source: ConnectedPeer,
                             modifierTypeId: ModifierTypeId,
                             modifierId: ModifierId)

    case class OtherNodeSyncingStatus[SI <: SyncInfo](remote: ConnectedPeer,
                                                      status: encry.consensus.History.HistoryComparisonResult,
                                                      extension: Option[Seq[(ModifierTypeId, ModifierId)]])

    trait PeerManagerEvent

    case class HandshakedPeer(remote: ConnectedPeer) extends PeerManagerEvent

    case class DisconnectedPeer(remote: InetSocketAddress) extends PeerManagerEvent

    trait NodeViewHolderEvent

    trait NodeViewChange extends NodeViewHolderEvent

    case class ChangedHistory[HR <: HistoryReader[_ <: PersistentNodeViewModifier, _ <: SyncInfo]](reader: HR) extends NodeViewChange

    //TODO: return mempool reader
    case class ChangedMempool[MR <: MempoolReader[_ <: Transaction[_]]](mempool: MR) extends NodeViewChange

    case class ChangedState[SR <: StateReader](reader: SR) extends NodeViewChange

    case class RollbackFailed(branchPointOpt: Option[VersionTag]) extends NodeViewHolderEvent

    case class RollbackSucceed(branchPointOpt: Option[VersionTag]) extends NodeViewHolderEvent

    case class NewOpenSurface(newSurface: Seq[ModifierId]) extends NodeViewHolderEvent

    case class StartingPersistentModifierApplication[PMOD <: PersistentNodeViewModifier](modifier: PMOD) extends NodeViewHolderEvent

    trait ModificationOutcome extends NodeViewHolderEvent

    case class FailedTransaction[P <: Proposition, TX <: Transaction[P]](transaction: TX, error: Throwable) extends ModificationOutcome

    case class SuccessfulTransaction[P <: Proposition, TX <: Transaction[P]](transaction: TX) extends ModificationOutcome

    case class SyntacticallyFailedModification[PMOD <: PersistentNodeViewModifier](modifier: PMOD, error: Throwable) extends ModificationOutcome

    case class SemanticallyFailedModification[PMOD <: PersistentNodeViewModifier](modifier: PMOD, error: Throwable) extends ModificationOutcome

    case class SyntacticallySuccessfulModifier[PMOD <: PersistentNodeViewModifier](modifier: PMOD) extends ModificationOutcome

    case class SemanticallySuccessfulModifier[PMOD <: PersistentNodeViewModifier](modifier: PMOD) extends ModificationOutcome

  }

}