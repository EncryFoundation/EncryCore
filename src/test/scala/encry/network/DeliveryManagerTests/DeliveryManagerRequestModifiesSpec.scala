package encry.network.DeliveryManagerTests

import java.net.InetSocketAddress
import akka.actor.ActorSystem
import encry.consensus.History.Older
import encry.modifiers.InstanceFactory
import encry.modifiers.history.{Block, Header, Payload}
import encry.network.BasicMessagesRepo.{Handshake, ModifiersNetworkMessage}
import encry.network.NetworkController.ReceivableMessages.DataFromPeer
import encry.network.NodeViewSynchronizer.ReceivableMessages.{HandshakedPeer, OtherNodeSyncingStatus, RequestFromLocal}
import encry.network.PeerConnectionHandler.{ConnectedPeer, Incoming}
import encry.settings.EncryAppSettings
import encry.utils.CoreTaggedTypes.ModifierId
import encry.view.EncryNodeViewHolder.DownloadRequest
import encry.view.history.EncryHistory
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import encry.network.DeliveryManagerTests.DMUtils._

class DeliveryManagerRequestModifiesSpec extends WordSpecLike with BeforeAndAfterAll
  with Matchers
  with InstanceFactory {

  implicit val system: ActorSystem = ActorSystem("SynchronousTestingSpec")
  val settings: EncryAppSettings = EncryAppSettings.read

  override def afterAll(): Unit = system.terminate()

  "RequestModifies" should {
    val initialState = initialiseDeliveryManager(isBlockChainSynced = true, isMining = true, settings, system)
    val deliveryManager = initialState._1
    val newPeer = new InetSocketAddress("172.16.13.10", 9001)
    val peer: ConnectedPeer = ConnectedPeer(newPeer, deliveryManager, Incoming,
      Handshake(protocolToBytes(settings.network.appVersion), "peer", Some(newPeer), System.currentTimeMillis()))
    val blocks: (EncryHistory, List[Block]) = generateBlocks(10, generateDummyHistory(settings))
    val headerIds: List[ModifierId] = blocks._2.map(_.header.id)
    val wrappedIds = headerIds.map(toKey)
    deliveryManager ! HandshakedPeer(peer)
    deliveryManager ! OtherNodeSyncingStatus(peer, Older, None)

    "handle uniq modifiers from RequestFromLocal message correctly" in {
      deliveryManager ! RequestFromLocal(peer, Header.modifierTypeId, headerIds)
      assert(deliveryManager.underlyingActor.expectedModifiers.getOrElse(peer.socketAddress.getAddress, Map.empty)
        .keys.size == headerIds.size)
      assert(deliveryManager.underlyingActor.expectedModifiers.getOrElse(peer.socketAddress.getAddress, Map.empty)
        .keys.forall(elem => wrappedIds.contains(elem)))
    }
    "not handle repeating modifiers from RequestFromLocal message" in {
      deliveryManager ! RequestFromLocal(peer, Header.modifierTypeId, headerIds)
      assert(deliveryManager.underlyingActor.expectedModifiers.getOrElse(peer.socketAddress.getAddress, Map.empty)
        .keys.size == headerIds.size)
      assert(deliveryManager.underlyingActor.expectedModifiers.getOrElse(peer.socketAddress.getAddress, Map.empty)
        .keys.forall(elem => wrappedIds.contains(elem)))
    }
    "Delivery manager should handle received requested modifier correctly" in {
      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(
        Header.modifierTypeId -> blocks._2.map(k => k.header.id -> Array.emptyByteArray).toMap), peer)
      assert(deliveryManager.underlyingActor.receivedModifiers.getOrElse(peer.socketAddress.getAddress, Set.empty)
        .size == headerIds.size)
      assert(deliveryManager.underlyingActor.receivedModifiers.getOrElse(peer.socketAddress.getAddress, Set.empty)
        .forall(elem => wrappedIds.contains(elem)))
      assert(deliveryManager.underlyingActor.headersForPriorityRequest.forall(x => wrappedIds.contains(x._1)))
    }
    "Delivery manager should not handle received repeating modifiers" in {
      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(
        Header.modifierTypeId -> blocks._2.map(k => k.header.id -> Array.emptyByteArray).toMap), peer)
      assert(deliveryManager.underlyingActor.receivedModifiers.getOrElse(peer.socketAddress.getAddress, Set.empty)
        .size == headerIds.size)
      assert(deliveryManager.underlyingActor.receivedModifiers.getOrElse(peer.socketAddress.getAddress, Set.empty)
        .forall(elem => wrappedIds.contains(elem)))
      assert(deliveryManager.underlyingActor.headersForPriorityRequest.forall(x => wrappedIds.contains(x._1)))
    }
    "handle priority request for payload correctly" in {
      headerIds.foreach(id =>
        deliveryManager ! DownloadRequest(Payload.modifierTypeId, blocks._2
          .find(block => block.id.sameElements(id)).get.payload.id, Some(id)))
      assert(deliveryManager.underlyingActor.expectedModifiers.getOrElse(peer.socketAddress.getAddress, Map.empty)
        .size == blocks._2.size)
      assert(deliveryManager.underlyingActor.headersForPriorityRequest.isEmpty)
    }
  }
}