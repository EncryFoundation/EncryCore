package encry.network

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.History._
import encry.consensus.SyncInfo
import encry.local.miner.Miner.{DisableMining, StartMining}
import encry.modifiers.history._
import encry.modifiers.mempool.{Transaction, TransactionProtoSerializer}
import encry.modifiers.{NodeViewModifier, PersistentNodeViewModifier}
import encry.network.AuxiliaryHistoryHolder.AuxHistoryChanged
import encry.network.DeliveryManager.FullBlockChainSynced
import BasicMessagesRepo._
import encry.network.NetworkController.ReceivableMessages.{DataFromPeer, RegisterMessagesHandler, SendToNetwork}
import encry.network.NodeViewSynchronizer.ReceivableMessages._
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.settings.EncryAppSettings
import encry.utils.CoreTaggedTypes.{ModifierId, ModifierTypeId, VersionTag}
import encry.utils.Utils._
import encry.view.EncryNodeViewHolder.DownloadRequest
import encry.view.EncryNodeViewHolder.ReceivableMessages.{CompareViews, GetNodeViewChanges}
import encry.view.history.{EncryHistory, EncryHistoryReader}
import encry.view.mempool.{Mempool, MempoolReader}
import encry.view.state.StateReader
import org.encryfoundation.common.Algos
import org.encryfoundation.common.transaction.Proposition

class NodeViewSynchronizer(influxRef: Option[ActorRef],
                           nodeViewHolderRef: ActorRef,
                           networkControllerRef: ActorRef,
                           system: ActorSystem,
                           settings: EncryAppSettings) extends Actor with StrictLogging {

  var historyReaderOpt: Option[EncryHistory] = None
  var mempoolReaderOpt: Option[Mempool] = None
  var modifiersRequestCache: Map[String, NodeViewModifier] = Map.empty
  var chainSynced: Boolean = false
  val deliveryManager: ActorRef = context.actorOf(
    Props(classOf[DeliveryManager], influxRef, nodeViewHolderRef, networkControllerRef, system, settings),
    "deliveryManager")

  override def preStart(): Unit = {
    val messageIds: Seq[Byte] =
      Seq(NetworkMessagesIds.Inv, NetworkMessagesIds.RequestModifier, NetworkMessagesIds.SyncInfo)
    networkControllerRef ! RegisterMessagesHandler(messageIds, self)
    context.system.eventStream.subscribe(self, classOf[NodeViewChange])
    context.system.eventStream.subscribe(self, classOf[ModificationOutcome])
    nodeViewHolderRef ! GetNodeViewChanges(history = true, state = false, vault = false, mempool = true)
  }

  override def receive: Receive = {
    case DownloadRequest(modifierTypeId: ModifierTypeId, modifierId: ModifierId, previousModifier: Option[ModifierId]) =>
      deliveryManager ! DownloadRequest(modifierTypeId, modifierId, previousModifier)
    case SuccessfulTransaction(tx) => broadcastModifierInv(tx)
    case SemanticallyFailedModification(_, _) =>
    case ChangedState(_) =>
    case SyntacticallyFailedModification(_, _) =>
    case SemanticallySuccessfulModifier(mod) =>
      mod match {
        case block: Block => broadcastModifierInv(block.header)
        case tx: Transaction => broadcastModifierInv(tx)
        case _ => //Do nothing
      }
    case AuxHistoryChanged(history) => historyReaderOpt = Some(history)
    case ChangedHistory(reader: EncryHistory@unchecked) if reader.isInstanceOf[EncryHistory] =>
      deliveryManager ! ChangedHistory(reader)
    case ChangedMempool(reader: Mempool) if reader.isInstanceOf[Mempool] =>
      mempoolReaderOpt = Some(reader)
    case HandshakedPeer(remote) => deliveryManager ! HandshakedPeer(remote)
    case DisconnectedPeer(remote) => deliveryManager ! DisconnectedPeer(remote)
    case DataFromPeer(message, remote) =>
      logger.info(s"Got ${message.messageName} on NodeViewSyncronizer.")
      message match {
        case SyncInfoNetworkMessage(syncInfo) =>
          logger.info(s"Got sync message from ${remote.socketAddress} with " +
            s"${syncInfo.lastHeaderIds.size} headers. Head's headerId is: " +
            s"${Algos.encode(syncInfo.lastHeaderIds.headOption.getOrElse(Array.emptyByteArray))}.")
          historyReaderOpt match {
            case Some(historyReader) =>
              val extensionOpt: Option[ModifierIds] = historyReader.continuationIds(syncInfo, settings.network.networkChunkSize)
              val ext: ModifierIds = extensionOpt.getOrElse(Seq())
              val comparison: HistoryComparisonResult = historyReader.compare(syncInfo)
              logger.info(s"Comparison with $remote having starting points ${idsToString(syncInfo.startingPoints)}. " +
                s"Comparison result is $comparison. Sending extension of length ${ext.length}")
              if (!(extensionOpt.nonEmpty || comparison != Younger)) logger.warn("Extension is empty while comparison is younger")
              deliveryManager ! OtherNodeSyncingStatus(remote, comparison, extensionOpt)
            case _ =>
          }
        case RequestModifiersNetworkMessage(invData) =>
          logger.info(s"Get request modifiers from remote peer. chainSynced = $chainSynced")
          if (chainSynced) {
            val inRequestCache: Map[String, NodeViewModifier] =
              invData._2.flatMap(id => modifiersRequestCache.get(Algos.encode(id)).map(mod => Algos.encode(mod.id) -> mod)).toMap
            logger.debug(s"inRequestCache(${inRequestCache.size}): ${inRequestCache.keys.mkString(",")}")
            self ! ResponseFromLocal(remote, invData._1, inRequestCache.values.toSeq)
            val nonInRequestCache = invData._2.filterNot(id => inRequestCache.contains(Algos.encode(id)))
            if (nonInRequestCache.nonEmpty)
              historyReaderOpt.flatMap(h => mempoolReaderOpt.map(mp => (h, mp))).foreach { readers =>
                val objs: Seq[NodeViewModifier] = invData._1 match {
                  case typeId: ModifierTypeId if typeId == Transaction.ModifierTypeId => readers._2.getAll(nonInRequestCache)
                  case _: ModifierTypeId => nonInRequestCache.flatMap(id => readers._1.modifierById(id))
                }
                logger.debug(s"nonInRequestCache(${objs.size}): ${objs.map(mod => Algos.encode(mod.id)).mkString(",")}")
                logger.debug(s"Requested ${invData._2.length} modifiers ${idsToString(invData)}, " +
                  s"sending ${objs.length} modifiers ${idsToString(invData._1, objs.map(_.id))} ")
                self ! ResponseFromLocal(remote, invData._1, objs)
              }
          }
          else logger.info(s"Peer $remote requested ${invData._2.length} modifiers ${idsToString(invData)}, but " +
            s"node is not synced, so ignore msg")
        case InvNetworkMessage(invData) =>
          logger.info(s"Got inv message from ${remote.socketAddress} with modifiers: ${invData._2.map(Algos.encode).mkString(",")} ")
          //todo: Ban node that send payload id?
          if (invData._1 != Payload.modifierTypeId) nodeViewHolderRef ! CompareViews(remote, invData._1, invData._2)
        case _ => logger.info(s"NodeViewSyncronyzer got invalid type of DataFromPeer message!")
      }
    case RequestFromLocal(peer, modifierTypeId, modifierIds) =>
      deliveryManager ! RequestFromLocal(peer, modifierTypeId, modifierIds)
    case StartMining => deliveryManager ! StartMining
    case DisableMining => deliveryManager ! DisableMining
    case FullBlockChainSynced =>
      chainSynced = true
      deliveryManager ! FullBlockChainSynced
    case ResponseFromLocal(peer, typeId, modifiers: Seq[NodeViewModifier]) =>
      if (modifiers.nonEmpty) typeId match {
        case _ if typeId == Header.modifierTypeId =>
          val modsB: Seq[(ModifierId, Array[Byte])] =
            modifiers.map { case h: Header => h.id -> HeaderProtoSerializer.toProto(h).toByteArray }
          peer.handlerRef ! ModifiersNetworkMessage(modifiers.head.modifierTypeId -> modsB.toMap)
        case _ if typeId == Payload.modifierTypeId =>
          val modsB: Seq[(ModifierId, Array[Byte])] =
            modifiers.map { case h: Payload => h.id -> PayloadProtoSerializer.toProto(h).toByteArray }
          peer.handlerRef ! ModifiersNetworkMessage(modifiers.head.modifierTypeId -> modsB.toMap)
        case _ if typeId == Transaction.ModifierTypeId =>
          peer.handlerRef ! ModifiersNetworkMessage(modifiers.head.modifierTypeId -> modifiers.map {
            case h: Transaction => h.id -> TransactionProtoSerializer.toProto(h).toByteArray
          }.toMap)
      }
    case a: Any => logger.error(s"Strange input(sender: ${sender()}): ${a.getClass}\n" + a)
  }

  def broadcastModifierInv[M <: NodeViewModifier](m: M): Unit =
    if (chainSynced) networkControllerRef ! SendToNetwork(InvNetworkMessage(m.modifierTypeId -> Seq(m.id)), Broadcast)
}

object NodeViewSynchronizer {

  object ReceivableMessages {

    case object SendLocalSyncInfo

    case object CheckModifiersToDownload

    case class OtherNodeSyncingStatus[SI <: SyncInfo](remote: ConnectedPeer,
                                                      status: encry.consensus.History.HistoryComparisonResult,
                                                      extension: Option[Seq[(ModifierTypeId, ModifierId)]])

    case class ResponseFromLocal[M <: NodeViewModifier]
    (source: ConnectedPeer, modifierTypeId: ModifierTypeId, localObjects: Seq[M])

    case class RequestFromLocal(source: ConnectedPeer, modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId])

    case class CheckDelivery(source: ConnectedPeer,
                             modifierTypeId: ModifierTypeId,
                             modifierId: ModifierId)

    trait PeerManagerEvent

    case class DisconnectedPeer(remote: InetSocketAddress) extends PeerManagerEvent

    case class HandshakedPeer(remote: ConnectedPeer) extends PeerManagerEvent

    trait NodeViewHolderEvent

    trait NodeViewChange extends NodeViewHolderEvent

    case class ChangedHistory[HR <: EncryHistoryReader](reader: HR) extends NodeViewChange

    case class ChangedMempool[MR <: MempoolReader](mempool: MR) extends NodeViewChange

    case class ChangedState[SR <: StateReader](reader: SR) extends NodeViewChange

    case class RollbackFailed(branchPointOpt: Option[VersionTag]) extends NodeViewHolderEvent

    case class RollbackSucceed(branchPointOpt: Option[VersionTag]) extends NodeViewHolderEvent

    trait ModificationOutcome extends NodeViewHolderEvent

    case class SyntacticallyFailedModification[PMOD <: PersistentNodeViewModifier](modifier: PMOD, error: Throwable)
      extends ModificationOutcome

    case class SemanticallyFailedModification[PMOD <: PersistentNodeViewModifier](modifier: PMOD, error: Throwable)
      extends ModificationOutcome

    case class SuccessfulTransaction[P <: Proposition, TX <: Transaction](transaction: TX) extends ModificationOutcome

    case class SemanticallySuccessfulModifier[PMOD <: PersistentNodeViewModifier](modifier: PMOD) extends ModificationOutcome

  }

}