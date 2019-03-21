package encry.network

import java.net.InetSocketAddress

import akka.testkit.{TestActorRef, TestProbe}
import akka.actor.ActorSystem
import encry.consensus.History.Older
import encry.local.miner.Miner.StartMining
import encry.modifiers.InstanceFactory
import encry.modifiers.history.{Block, Header, Payload}
import encry.network.BasicMessagesRepo.{Handshake, ModifiersNetworkMessage}
import encry.network.DeliveryManager.FullBlockChainSynced
import encry.network.NetworkController.ReceivableMessages.DataFromPeer
import encry.network.NodeViewSynchronizer.ReceivableMessages.{HandshakedPeer, HistoryChanges, OtherNodeSyncingStatus, RequestFromLocal}
import encry.network.PeerConnectionHandler.{ConnectedPeer, Incoming}
import encry.settings.EncryAppSettings
import encry.utils.CoreTaggedTypes.ModifierId
import encry.view.history.EncryHistory
import org.scalatest.{BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike}
import encry.utils.TestHelper._
import encry.utils.FileHelper._
import encry.view.EncryNodeViewHolder.DownloadRequest

import scala.collection.{immutable, mutable}
import scala.collection.mutable.WrappedArray
import scala.util.Try

class DeliveryManagerSpec extends WordSpecLike with BeforeAndAfterAll
  with Matchers
  with InstanceFactory
  with OneInstancePerTest {

  implicit val system: ActorSystem = ActorSystem("SynchronousTestingSpec")
  val settings: EncryAppSettings = EncryAppSettings.read

  override def afterAll(): Unit = system.terminate()


  "DeliveryManager" should {
    "handle only HistoryChange message before first history initialising and " +
      "stash all messages instead of HistoryChanges before first history initialising" in {
      val history: EncryHistory = generateDummyHistory(settings)
      val deliveryManager: TestActorRef[DeliveryManager] =
        TestActorRef[DeliveryManager](DeliveryManager.props(None, TestProbe().ref, TestProbe().ref, settings))
      val newPeer = new InetSocketAddress("172.16.13.10", 9001)
      val newPeer2 = new InetSocketAddress("172.16.13.11", 9001)
      val peer: ConnectedPeer = ConnectedPeer(newPeer, deliveryManager, Incoming,
        Handshake(protocolToBytes(settings.network.appVersion), "peer", Some(newPeer), System.currentTimeMillis()))
      val peer2: ConnectedPeer = ConnectedPeer(newPeer2, deliveryManager, Incoming,
        Handshake(protocolToBytes(settings.network.appVersion), "peer", Some(newPeer2), System.currentTimeMillis()))

      deliveryManager ! HandshakedPeer(peer)
      assert(deliveryManager.underlyingActor.syncTracker.statuses.isEmpty)
      deliveryManager ! HistoryChanges(history)
      deliveryManager ! HandshakedPeer(peer2)
      assert(deliveryManager.underlyingActor.syncTracker.statuses.size == 2)
      deliveryManager.stop()
    }
    "RequestModifies" should {
      "correct processed after messages" in {
        val initialState = initialiseDeliveryManager(isBlockChainSynced = true, isMining = true)
        val deliveryManager = initialState._1
        val newPeer = new InetSocketAddress("172.16.13.10", 9001)
        val peer: ConnectedPeer = ConnectedPeer(newPeer, deliveryManager, Incoming,
          Handshake(protocolToBytes(settings.network.appVersion), "peer", Some(newPeer), System.currentTimeMillis()))
        val blocks: (EncryHistory, List[Block]) = generateBlocks(10, generateDummyHistory(settings))
        val headerIds: List[ModifierId] = blocks._2.map(_.header.id)
        val wrappedIds = headerIds.map(toKey)
        deliveryManager ! HandshakedPeer(peer)
        deliveryManager ! OtherNodeSyncingStatus(peer, Older, None)
        deliveryManager ! RequestFromLocal(peer, Header.modifierTypeId, headerIds)

        assert(deliveryManager.underlyingActor.expectedModifiers.getOrElse(peer.socketAddress.getAddress, Map.empty)
          .keys.size == headerIds.size)
        assert(deliveryManager.underlyingActor.expectedModifiers.getOrElse(peer.socketAddress.getAddress, Map.empty)
          .keys.forall(elem => wrappedIds.contains(elem)))

        deliveryManager ! RequestFromLocal(peer, Header.modifierTypeId, headerIds)
        assert(deliveryManager.underlyingActor.expectedModifiers.getOrElse(peer.socketAddress.getAddress, Map.empty)
          .keys.size == headerIds.size)
        assert(deliveryManager.underlyingActor.expectedModifiers.getOrElse(peer.socketAddress.getAddress, Map.empty)
          .keys.forall(elem => wrappedIds.contains(elem)))

        deliveryManager ! DataFromPeer(ModifiersNetworkMessage(
          Header.modifierTypeId -> blocks._2.map(k => k.header.id -> Array.emptyByteArray).toMap), peer)
        assert(deliveryManager.underlyingActor.receivedModifiers.getOrElse(peer.socketAddress.getAddress, Set.empty)
          .size == headerIds.size)
        assert(deliveryManager.underlyingActor.receivedModifiers.getOrElse(peer.socketAddress.getAddress, Set.empty)
          .forall(elem => wrappedIds.contains(elem)))
        assert(deliveryManager.underlyingActor.headersForPriorityRequest.forall(x => wrappedIds.contains(x._1)))

        deliveryManager ! DataFromPeer(ModifiersNetworkMessage(
          Header.modifierTypeId -> blocks._2.map(k => k.header.id -> Array.emptyByteArray).toMap), peer)
        assert(deliveryManager.underlyingActor.receivedModifiers.getOrElse(peer.socketAddress.getAddress, Set.empty)
          .size == headerIds.size)
        assert(deliveryManager.underlyingActor.receivedModifiers.getOrElse(peer.socketAddress.getAddress, Set.empty)
          .forall(elem => wrappedIds.contains(elem)))
        assert(deliveryManager.underlyingActor.headersForPriorityRequest.forall(x => wrappedIds.contains(x._1)))

        headerIds.foreach(id =>
          deliveryManager ! DownloadRequest(Payload.modifierTypeId, blocks._2
            .find(block => block.id.sameElements(id)).get.payload.id, Some(id)))
        assert(deliveryManager.underlyingActor.expectedModifiers.getOrElse(peer.socketAddress.getAddress, Map.empty)
          .size == blocks._2.size)
        assert(deliveryManager.underlyingActor.headersForPriorityRequest.isEmpty)
      }
    }
    "ReRequestModifier" should {
      "correct processed after messages" in {

      }
    }
  }

  def initialiseDeliveryManager(isBlockChainSynced: Boolean, isMining: Boolean): (TestActorRef[DeliveryManager], EncryHistory) = {
    val history: EncryHistory = generateDummyHistory(settings)
    val deliveryManager: TestActorRef[DeliveryManager] =
      TestActorRef[DeliveryManager](DeliveryManager.props(None, TestProbe().ref, TestProbe().ref, settings))
    deliveryManager ! HistoryChanges(history)
    if (isMining) deliveryManager ! StartMining
    if (isBlockChainSynced) deliveryManager ! FullBlockChainSynced
    (deliveryManager, history)
  }

  def generateBlocks(qty: Int, history: EncryHistory): (EncryHistory, List[Block]) =
    (0 until qty).foldLeft(history, List.empty[Block]) {
      case ((prevHistory, blocks), _) =>
        val block: Block = generateNextBlock(prevHistory)
        (prevHistory.append(block.header).get._1.append(block.payload).get._1.reportModifierIsValid(block), blocks :+ block)
    }

  def toKey(id: ModifierId) = new mutable.WrappedArray.ofByte(id)

}
