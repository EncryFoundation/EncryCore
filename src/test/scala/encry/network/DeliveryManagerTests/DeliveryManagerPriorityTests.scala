package encry.network.DeliveryManagerTests

import java.net.InetSocketAddress
import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestKit}
import encry.consensus.History.{HistoryComparisonResult, Older}
import encry.modifiers.InstanceFactory
import encry.modifiers.history.{Block, Header, Payload}
import encry.network.BasicMessagesRepo.{Handshake, ModifiersNetworkMessage}
import encry.network.DeliveryManager
import encry.network.DeliveryManagerTests.DMUtils.{generateBlocks, initialiseDeliveryManager}
import encry.network.NetworkController.ReceivableMessages.DataFromPeer
import encry.network.NodeViewSynchronizer.ReceivableMessages.{HandshakedPeer, OtherNodeSyncingStatus, RequestFromLocal}
import encry.network.PeerConnectionHandler.{ConnectedPeer, Incoming}
import encry.network.SyncTracker.PeerPriorityStatus.PeerPriorityStatus
import encry.settings.EncryAppSettings
import org.scalatest.{BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike}

class DeliveryManagerPriorityTests extends WordSpecLike with BeforeAndAfterAll
  with Matchers
  with InstanceFactory
  with OneInstancePerTest {

  implicit val system: ActorSystem = ActorSystem("SynchronousTestingSpec")
  val settings: EncryAppSettings = DummyEncryAppSettingsReader.read

  override def afterAll: Unit = TestKit.shutdownActorSystem(system)

  "Delivery Manager" should {
    val initialState = initialiseDeliveryManager(isBlockChainSynced = true, isMining = true, settings)
    val deliveryManager: TestActorRef[DeliveryManager] = initialState._1
    val newPeer: InetSocketAddress = new InetSocketAddress("172.16.13.10", 9001)
    val peer: ConnectedPeer = ConnectedPeer(newPeer, deliveryManager, Incoming,
      Handshake(protocolToBytes(settings.network.appVersion), "peer", Some(newPeer), System.currentTimeMillis()))
    val blocks: List[Block] = generateBlocks(10, generateDummyHistory(settings))._2
    deliveryManager ! HandshakedPeer(peer)
    deliveryManager ! OtherNodeSyncingStatus(peer, Older, None)

    /**
      * This test simulates DeliveryManager behaviour connected with updating nodes priority while blockChain is not synced.
      *
      * Send handshake to the Delivery Manager from peer1.
      * Send downloadRequest for N modifiers to the Delivery manager.
      * Delivery manager must send requestModifier message for N modifiers to peer1.
      * Do not send any modifiers to the Delivery manager from peer1.
      * Check on Delivery manager that peer1 priority is BadNode(1).
      */

    "mark peer as BadNode with BadPriority (1)" in {
      deliveryManager ! RequestFromLocal(peer, Header.modifierTypeId, blocks.map(_.header.id))
      Thread.sleep(6000)
      val priorityResult: (HistoryComparisonResult, PeerPriorityStatus, ConnectedPeer) =
        deliveryManager.underlyingActor.syncTracker.statuses(newPeer.getAddress)
      assert(priorityResult._3 == peer)
      assert(priorityResult._1 == Older)
      assert(priorityResult._2 == 1)
    }

    /**
      * This test simulates DeliveryManager behaviour connected with updating nodes priority while blockChain is not synced.
      *
      * Send handshake to the Delivery Manager from peer1.
      * Send downloadRequest for N modifiers to the Delivery manager.
      * Delivery manager must send requestModifier message for N modifiers to peer1.
      * Send N valid requested modifiers to the Delivery manager from peer1.
      * Check on Delivery manager that peer1 priority is HighPriority(4).
      */

    "mark peer as BestNode with HighPriority (4)" in {
      deliveryManager ! RequestFromLocal(peer, Header.modifierTypeId, blocks.map(_.header.id))
      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(
        Header.modifierTypeId, blocks.map(block => block.header.id -> block.header.bytes).toMap), peer)
      Thread.sleep(6000)
      val priorityResult: (HistoryComparisonResult, PeerPriorityStatus, ConnectedPeer) =
        deliveryManager.underlyingActor.syncTracker.statuses(newPeer.getAddress)
      assert(priorityResult._3 == peer)
      assert(priorityResult._1 == Older)
      assert(priorityResult._2 == 4)
    }

    /**
      * This test simulates DeliveryManager behaviour connected with updating nodes priority while blockChain is not synced.
      *
      * Send handshake to the Delivery Manager from peer1.
      * Send downloadRequest for N modifiers to the Delivery manager.
      * Delivery manager must send requestModifier message for N modifiers to peer1.
      * Send N / 2 valid requested modifiers to the Delivery manager from peer1.
      * Check on Delivery manager that peer1 priority is LowPriority(3).
      */

    "mark peer as LowPriorityNode with LowPriority (3)" in {
      deliveryManager ! RequestFromLocal(peer, Header.modifierTypeId, blocks.map(_.header.id))
      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(
        Header.modifierTypeId, blocks.take(5).map(block => block.header.id -> block.header.bytes).toMap), peer)
      Thread.sleep(6000)
      val priorityResult: (HistoryComparisonResult, PeerPriorityStatus, ConnectedPeer) =
        deliveryManager.underlyingActor.syncTracker.statuses(newPeer.getAddress)
      assert(priorityResult._3 == peer)
      assert(priorityResult._1 == Older)
      assert(priorityResult._2 == 3)
    }

    /**
      * This test simulates DeliveryManager behaviour connected with updating nodes priority while blockChain synced.
      *
      * Send handshake to the Delivery Manager from peer1, peer2, peer3.
      * Send downloadRequest for N modifiers to the Delivery manager.
      * Delivery manager must send requestModifier message for N modifiers to all peers.
      * Send N valid requested modifiers to the Delivery manager from peer1.
      * Send N / 2 valid requested modifiers to the Delivery manager from peer2.
      * Send 0 valid requested modifiers to the Delivery manager from peer3.
      * Check on Delivery manager that peer1 priorities is HighPriority(4).
      * Check on Delivery manager that peer2 priorities is LowPriority(3).
      * Check on Delivery manager that peer3 priorities is BadNode(1).
      */

    "correctly choose peer priority while several peers are available" in {

      val newPeer1: InetSocketAddress = new InetSocketAddress("172.16.13.11", 9001)
      val peer1: ConnectedPeer = ConnectedPeer(newPeer1, deliveryManager, Incoming,
        Handshake(protocolToBytes(settings.network.appVersion), "peer1", Some(newPeer1), System.currentTimeMillis()))

      deliveryManager ! HandshakedPeer(peer1)
      deliveryManager ! OtherNodeSyncingStatus(peer1, Older, None)

      val newPeer2: InetSocketAddress = new InetSocketAddress("172.16.13.12", 9001)
      val peer2: ConnectedPeer = ConnectedPeer(newPeer2, deliveryManager, Incoming,
        Handshake(protocolToBytes(settings.network.appVersion), "peer2", Some(newPeer2), System.currentTimeMillis()))

      deliveryManager ! HandshakedPeer(peer2)
      deliveryManager ! OtherNodeSyncingStatus(peer2, Older, None)

      deliveryManager ! RequestFromLocal(peer, Header.modifierTypeId, blocks.map(_.header.id))
      deliveryManager ! RequestFromLocal(peer1, Header.modifierTypeId, blocks.map(_.header.id))
      deliveryManager ! RequestFromLocal(peer2, Header.modifierTypeId, blocks.map(_.header.id))

      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(
        Header.modifierTypeId, blocks.map(block => block.header.id -> block.header.bytes).toMap), peer)

      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(
        Header.modifierTypeId, blocks.take(5).map(block => block.header.id -> block.header.bytes).toMap), peer1)

      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(
        Header.modifierTypeId, blocks.take(2).map(block => block.header.id -> block.header.bytes).toMap), peer2)

      Thread.sleep(5000)

      val priorityResultByPeer: (HistoryComparisonResult, PeerPriorityStatus, ConnectedPeer) =
        deliveryManager.underlyingActor.syncTracker.statuses(newPeer.getAddress)

      val priorityResultByPeer1: (HistoryComparisonResult, PeerPriorityStatus, ConnectedPeer) =
        deliveryManager.underlyingActor.syncTracker.statuses(newPeer1.getAddress)

      val priorityResultByPeer2: (HistoryComparisonResult, PeerPriorityStatus, ConnectedPeer) =
        deliveryManager.underlyingActor.syncTracker.statuses(newPeer2.getAddress)

      assert(priorityResultByPeer._3 == peer)
      assert(priorityResultByPeer._1 == Older)
      assert(priorityResultByPeer._2 == 4)

      assert(priorityResultByPeer1._3 == peer1)
      assert(priorityResultByPeer1._1 == Older)
      assert(priorityResultByPeer1._2 == 3)

      assert(priorityResultByPeer2._3 == peer2)
      assert(priorityResultByPeer2._1 == Older)
      assert(priorityResultByPeer2._2 == 1)
    }

    /**
      * This test simulates DeliveryManager behaviour connected with the logic of choosing nodes relative to priority.
      *
      * First - send handshake message from peer1.
      * Send downloadRequest for N modifiers to the Delivery manager.
      * Delivery manager must send requestModifier message for N modifiers to peer1.
      * Send N valid requested modifiers to the Delivery manager from peer1.
      * Check on Delivery manager that peer1 priority is HighPriority(4).
      *
      * Second - send handshake message from peer2.
      * Send downloadRequest for N modifiers to the Delivery manager.
      * Delivery manager must send requestModifier message for M modifiers to peer1.
      * Send M - K where ((M - K) / M) < LowPriority valid requested modifiers to the Delivery manager from peer1.
      * Check on Delivery manager that peer1 priority is BadNode(1).
      *
      * Third - send downloadRequest for N modifiers to the delivery manager.
      * Delivery manager must send requestModifier message for M modifiers to peer2.
      * Send M valid requested modifiers to the Delivery manager from peer2.
      * Check peer1 priority on this node - must be BadNode(1).
      * Check peer2 priority on this node - must be HighPriority(4).
      */

    "shows right behaviour while several nodes are available" in {
      val newPeer1: InetSocketAddress = new InetSocketAddress("172.16.13.11", 9001)
      val peer1: ConnectedPeer = ConnectedPeer(newPeer1, deliveryManager, Incoming,
        Handshake(protocolToBytes(settings.network.appVersion), "peer1", Some(newPeer1), System.currentTimeMillis()))

      deliveryManager ! HandshakedPeer(peer1)
      deliveryManager ! OtherNodeSyncingStatus(peer1, Older, None)

      deliveryManager ! RequestFromLocal(peer, Header.modifierTypeId, blocks.map(_.header.id))

      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(
        Header.modifierTypeId, blocks.map(block => block.header.id -> block.header.bytes).toMap), peer)

      Thread.sleep(6000)

      val priorityResultByPeer: (HistoryComparisonResult, PeerPriorityStatus, ConnectedPeer) =
        deliveryManager.underlyingActor.syncTracker.statuses(newPeer.getAddress)

      val priorityResultByPeer1: (HistoryComparisonResult, PeerPriorityStatus, ConnectedPeer) =
        deliveryManager.underlyingActor.syncTracker.statuses(newPeer1.getAddress)

      assert(priorityResultByPeer._3 == peer)
      assert(priorityResultByPeer._1 == Older)
      assert(priorityResultByPeer._2 == 4)

      assert(priorityResultByPeer1._3 == peer1)
      assert(priorityResultByPeer1._1 == Older)
      assert(priorityResultByPeer1._2 == 2)

      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(
        Header.modifierTypeId, blocks.map(block => block.header.id -> block.header.bytes).toMap), peer1)

      Thread.sleep(6000)

      val priorityResultByPeerNew: (HistoryComparisonResult, PeerPriorityStatus, ConnectedPeer) =
        deliveryManager.underlyingActor.syncTracker.statuses(newPeer.getAddress)

      val priorityResultByPeer1New: (HistoryComparisonResult, PeerPriorityStatus, ConnectedPeer) =
        deliveryManager.underlyingActor.syncTracker.statuses(newPeer1.getAddress)

      assert(priorityResultByPeerNew._3 == peer)
      assert(priorityResultByPeerNew._1 == Older)
      assert(priorityResultByPeerNew._2 == 4)

      assert(priorityResultByPeer1New._3 == peer1)
      assert(priorityResultByPeer1New._1 == Older)
      assert(priorityResultByPeer1New._2 == 1)

      val newPeer2: InetSocketAddress = new InetSocketAddress("172.16.13.15", 9001)
      val peer2: ConnectedPeer = ConnectedPeer(newPeer2, deliveryManager, Incoming,
        Handshake(protocolToBytes(settings.network.appVersion), "peer15", Some(newPeer2), System.currentTimeMillis()))

      deliveryManager ! HandshakedPeer(peer2)
      deliveryManager ! OtherNodeSyncingStatus(peer2, Older, None)

      deliveryManager ! RequestFromLocal(peer2, Payload.modifierTypeId, blocks.map(_.payload.id))

      deliveryManager ! RequestFromLocal(peer, Payload.modifierTypeId, blocks.map(_.payload.id))

      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(
        Header.modifierTypeId, blocks.take(2).map(block => block.payload.id -> block.payload.bytes).toMap), peer2)

      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(
        Header.modifierTypeId, blocks.map(block => block.payload.id -> block.payload.bytes).toMap), peer)

      Thread.sleep(6000)

      val priorityResultByPeerNew1: (HistoryComparisonResult, PeerPriorityStatus, ConnectedPeer) =
        deliveryManager.underlyingActor.syncTracker.statuses(newPeer.getAddress)

      val priorityResultByPeer1New2: (HistoryComparisonResult, PeerPriorityStatus, ConnectedPeer) =
        deliveryManager.underlyingActor.syncTracker.statuses(newPeer2.getAddress)

      assert(priorityResultByPeerNew1._3 == peer)
      assert(priorityResultByPeerNew1._1 == Older)
      assert(priorityResultByPeerNew1._2 == 4)

      assert(priorityResultByPeer1New2._3 == peer2)
      assert(priorityResultByPeer1New2._1 == Older)
      assert(priorityResultByPeer1New2._2 == 1)
    }

    /**
      * This test simulates DeliveryManager behaviour connected with updating nodes priority while blockChain synced.
      *
      * Send handshake to the Delivery Manager from peer1, peer2, peer3.
      * Send downloadRequest for N modifiers to the Delivery manager.
      * Delivery manager must send requestModifier message for N modifiers to all peers.
      * Send N valid requested modifiers to the Delivery manager from all peers.
      * Check on Delivery manager that peer1, peer2, peer3 priorities is HighPriority(4).
      */

    "shows define same priorities correctly" in {
      val newPeer1: InetSocketAddress = new InetSocketAddress("172.16.13.11", 9001)
      val peer1: ConnectedPeer = ConnectedPeer(newPeer1, deliveryManager, Incoming,
        Handshake(protocolToBytes(settings.network.appVersion), "peer1", Some(newPeer1), System.currentTimeMillis()))

      deliveryManager ! HandshakedPeer(peer1)
      deliveryManager ! OtherNodeSyncingStatus(peer1, Older, None)

      val newPeer2: InetSocketAddress = new InetSocketAddress("172.16.13.12", 9001)
      val peer2: ConnectedPeer = ConnectedPeer(newPeer2, deliveryManager, Incoming,
        Handshake(protocolToBytes(settings.network.appVersion), "peer2", Some(newPeer2), System.currentTimeMillis()))

      deliveryManager ! HandshakedPeer(peer2)
      deliveryManager ! OtherNodeSyncingStatus(peer2, Older, None)

      deliveryManager ! RequestFromLocal(peer, Header.modifierTypeId, blocks.map(_.header.id))
      deliveryManager ! RequestFromLocal(peer1, Header.modifierTypeId, blocks.map(_.header.id))
      deliveryManager ! RequestFromLocal(peer2, Header.modifierTypeId, blocks.map(_.header.id))

      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(
        Header.modifierTypeId, blocks.map(block => block.header.id -> block.header.bytes).toMap), peer)

      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(
        Header.modifierTypeId, blocks.map(block => block.header.id -> block.header.bytes).toMap), peer1)

      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(
        Header.modifierTypeId, blocks.map(block => block.header.id -> block.header.bytes).toMap), peer2)

      Thread.sleep(6000)

      val priorityResultByPeer: (HistoryComparisonResult, PeerPriorityStatus, ConnectedPeer) =
        deliveryManager.underlyingActor.syncTracker.statuses(newPeer.getAddress)

      val priorityResultByPeer1: (HistoryComparisonResult, PeerPriorityStatus, ConnectedPeer) =
        deliveryManager.underlyingActor.syncTracker.statuses(newPeer1.getAddress)

      val priorityResultByPeer2: (HistoryComparisonResult, PeerPriorityStatus, ConnectedPeer) =
        deliveryManager.underlyingActor.syncTracker.statuses(newPeer2.getAddress)

      assert(priorityResultByPeer._3 == peer)
      assert(priorityResultByPeer._1 == Older)
      assert(priorityResultByPeer._2 == 4)

      assert(priorityResultByPeer1._3 == peer1)
      assert(priorityResultByPeer1._1 == Older)
      assert(priorityResultByPeer1._2 == 4)

      assert(priorityResultByPeer2._3 == peer2)
      assert(priorityResultByPeer2._1 == Older)
      assert(priorityResultByPeer2._2 == 4)
    }

    "not increment modifiers which will be putted in spam collection" in {
      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(
        Header.modifierTypeId, blocks.map(block => block.header.id -> block.header.bytes).toMap), peer)

      Thread.sleep(6000)

      val priorityResultByPeer: (HistoryComparisonResult, PeerPriorityStatus, ConnectedPeer) =
        deliveryManager.underlyingActor.syncTracker.statuses(newPeer.getAddress)

      assert(priorityResultByPeer._3 == peer)
      assert(priorityResultByPeer._1 == Older)
      assert(priorityResultByPeer._2 == 1)
    }
  }
}