package encry.network.DeliveryManagerTests

import java.net.InetSocketAddress
import encry.network.DeliveryManagerTests.DMUtils.{createPeer, generateBlocks, initialiseDeliveryManager}
import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestKit}
import encry.consensus.History.{HistoryComparisonResult, Older}
import encry.modifiers.InstanceFactory
import encry.modifiers.history.{Block, Header}
import encry.network.BasicMessagesRepo.ModifiersNetworkMessage
import encry.network.DeliveryManager
import encry.network.NetworkController.ReceivableMessages.DataFromPeer
import encry.network.NodeViewSynchronizer.ReceivableMessages.{HandshakedPeer, OtherNodeSyncingStatus, RequestFromLocal}
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.network.SyncTracker.PeerPriorityStatus.PeerPriorityStatus
import encry.settings.EncryAppSettings
import encry.utils.CoreTaggedTypes.ModifierId
import org.scalatest.{BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike}

class DeliveryManagerPriorityTests extends WordSpecLike
  with BeforeAndAfterAll
  with Matchers
  with InstanceFactory
  with OneInstancePerTest {

  implicit val system: ActorSystem = ActorSystem("SynchronousTestingSpec")
  val settings: EncryAppSettings = DummyEncryAppSettingsReader.read

  override def afterAll: Unit = TestKit.shutdownActorSystem(system)

  def initialiseState: (TestActorRef[DeliveryManager], ConnectedPeer, ConnectedPeer, ConnectedPeer,
    ConnectedPeer, ConnectedPeer, ConnectedPeer, List[Block], List[ModifierId]) = {
    val (deliveryManager, _) = initialiseDeliveryManager(isBlockChainSynced = true, isMining = true, settings)
    val (_: InetSocketAddress, cp1: ConnectedPeer) = createPeer(9001, "172.16.13.10", settings)
    val (_: InetSocketAddress, cp2: ConnectedPeer) = createPeer(9002, "172.16.13.11", settings)
    val (_: InetSocketAddress, cp3: ConnectedPeer) = createPeer(9003, "172.16.13.12", settings)
    val (_: InetSocketAddress, cp4: ConnectedPeer) = createPeer(9004, "172.16.13.13", settings)
    val (_: InetSocketAddress, cp5: ConnectedPeer) = createPeer(9005, "172.16.13.14", settings)
    val (_: InetSocketAddress, cp6: ConnectedPeer) = createPeer(9006, "172.16.13.15", settings)
    val blocks: List[Block] = generateBlocks(10, generateDummyHistory(settings))._2
    val headersIds: List[ModifierId] = blocks.map(_.header.id)
    (deliveryManager, cp1, cp2, cp3, cp4, cp5, cp6, blocks, headersIds)
  }

  "Delivery Manager" should {
    /**
      * This test simulates DeliveryManager behaviour connected with updating nodes priority.
      *
      * Test expected behavior is:
      * Send handshake to the Delivery Manager from cp1 for cp1.
      * Send RequestFromLocal for N modifiers to the Delivery Manager.
      * Delivery manager have to use requestModifier, send request to N modifiers to cp1 and put this N modifiers in expectedModifiersCollection.
      * Receive less than 1/2 of this modifiers during 1 attempt.
      * When period of updating priorities will expire, delivery manager will mark cp1 as BadNode.
      *
      */
    "mark peer as BadNode with BadPriority (1)" in {
      val (deliveryManager, cp1, _, _, _, _, _, _, headersIds) = initialiseState
      deliveryManager ! HandshakedPeer(cp1)
      deliveryManager ! OtherNodeSyncingStatus(cp1, Older, None)
      deliveryManager ! RequestFromLocal(cp1, Header.modifierTypeId, headersIds)
      deliveryManager.underlyingActor.syncTracker.updatePeersPriorityStatus()
      val priorityResult: (HistoryComparisonResult, PeerPriorityStatus, ConnectedPeer) =
        deliveryManager.underlyingActor.syncTracker.statuses(cp1.socketAddress.getAddress)
      assert(priorityResult._3 == cp1)
      assert(priorityResult._1 == Older)
      assert(priorityResult._2 == 1)
      deliveryManager.stop()
    }

    /**
      * This test simulates DeliveryManager behaviour connected with updating nodes priority
      *
      * Test expected behavior is:
      * Send handshake to the Delivery Manager from cp1.
      * Send RequestFromLocal for N modifiers to the Delivery Manager for cp1.
      * Delivery manager have to use requestModifier, send request to N modifiers to cp1 and put this N modifiers in expectedModifiersCollection.
      * Receive more than 3\4 of this modifiers during 1 attempt.
      * When period of updating priorities will expire, delivery manager will mark cp1 as BestNode.
      */
    "mark peer as HighPriorityNode with HighPriority (4)" in {
      val (deliveryManager, cp1, _, _, _, _, _, blocks, headersIds) = initialiseState
      deliveryManager ! HandshakedPeer(cp1)
      deliveryManager ! OtherNodeSyncingStatus(cp1, Older, None)
      deliveryManager ! RequestFromLocal(cp1, Header.modifierTypeId, headersIds)
      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(
        Header.modifierTypeId, blocks.map(block => block.header.id -> block.header.bytes).toMap), cp1)
      deliveryManager.underlyingActor.syncTracker.updatePeersPriorityStatus()
      val priorityResult: (HistoryComparisonResult, PeerPriorityStatus, ConnectedPeer) =
        deliveryManager.underlyingActor.syncTracker.statuses(cp1.socketAddress.getAddress)
      assert(priorityResult._3 == cp1)
      assert(priorityResult._1 == Older)
      assert(priorityResult._2 == 4)
      deliveryManager.stop()
    }

    /**
      * This test simulates DeliveryManager behaviour connected with updating nodes priority
      *
      * Test expected behavior is:
      * Send handshake to the Delivery Manager from cp1.
      * Send RequestFromLocal for N modifiers to the Delivery Manager for cp1.
      * Delivery manager have to use requestModifier, send request to N modifiers to cp1 and put this N modifiers in expectedModifiersCollection.
      * Receive more than 1\2 and less than 3\4 of this modifiers during 1 attempt.
      * When period of updating priorities will expire, delivery manager will mark cp1 as LowPriorityNode.
      */
    "mark peer as LowPriorityNode with LowPriority (3)" in {
      val (deliveryManager, cp1, _, _, _, _, _, blocks, headersIds) = initialiseState
      deliveryManager ! HandshakedPeer(cp1)
      deliveryManager ! OtherNodeSyncingStatus(cp1, Older, None)
      deliveryManager ! RequestFromLocal(cp1, Header.modifierTypeId, headersIds)
      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(
        Header.modifierTypeId, blocks.take(6).map(block => block.header.id -> block.header.bytes).toMap), cp1)
      deliveryManager.underlyingActor.syncTracker.updatePeersPriorityStatus()
      val priorityResult: (HistoryComparisonResult, PeerPriorityStatus, ConnectedPeer) =
        deliveryManager.underlyingActor.syncTracker.statuses(cp1.socketAddress.getAddress)
      assert(priorityResult._3 == cp1)
      assert(priorityResult._1 == Older)
      assert(priorityResult._2 == 3)
      deliveryManager.stop()
    }

    /**
      * This test simulates DeliveryManager behavior connected with updating several nodes priority active in one time
      *
      * Test expected behavior is:
      * Send handshake to the Delivery Manager from cp1, cp2, cp3, cp4, cp5, cp6.
      * Send RequestFromLocal for N modifiers to the Delivery Manager for cp1, cp2, cp3, cp4, cp5, cp6.
      * Delivery manager have to use requestModifier, send request to N modifiers to cp1 and put this N modifiers in expectedModifiersCollection.
      * Receive more than 3\4 requested modifiers from cp1 and cp4.
      * Receive less than 3\4 but more than 1\2 requested modifiers from cp2 and cp5.
      * Receive less than 1\2 requested modifiers from cp3 and cp6.
      * When period of updating priorities will expire, delivery manager will mark cp1 and cp4 as HighPriorityNode.
      * When period of updating priorities will expire, delivery manager will mark cp2 and cp5 as LowPriorityNode.
      * When period of updating priorities will expire, delivery manager will mark cp3 and cp6 as BadNode.
      */
    "correctly choose peer priority while several peers are available" in {
      val (deliveryManager, cp1, cp2, cp3, cp4, cp5, cp6, blocks, headersIds) = initialiseState
      deliveryManager ! HandshakedPeer(cp1)
      deliveryManager ! OtherNodeSyncingStatus(cp1, Older, None)
      deliveryManager ! HandshakedPeer(cp2)
      deliveryManager ! OtherNodeSyncingStatus(cp2, Older, None)
      deliveryManager ! HandshakedPeer(cp3)
      deliveryManager ! OtherNodeSyncingStatus(cp3, Older, None)
      deliveryManager ! HandshakedPeer(cp4)
      deliveryManager ! OtherNodeSyncingStatus(cp4, Older, None)
      deliveryManager ! HandshakedPeer(cp5)
      deliveryManager ! OtherNodeSyncingStatus(cp5, Older, None)
      deliveryManager ! HandshakedPeer(cp6)
      deliveryManager ! OtherNodeSyncingStatus(cp6, Older, None)
      deliveryManager ! RequestFromLocal(cp1, Header.modifierTypeId, headersIds)
      deliveryManager ! RequestFromLocal(cp2, Header.modifierTypeId, headersIds)
      deliveryManager ! RequestFromLocal(cp3, Header.modifierTypeId, headersIds)
      deliveryManager ! RequestFromLocal(cp4, Header.modifierTypeId, headersIds)
      deliveryManager ! RequestFromLocal(cp5, Header.modifierTypeId, headersIds)
      deliveryManager ! RequestFromLocal(cp6, Header.modifierTypeId, headersIds)
      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(
        Header.modifierTypeId, blocks.map(block => block.header.id -> block.header.bytes).toMap), cp1)
      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(
        Header.modifierTypeId, blocks.map(block => block.header.id -> block.header.bytes).toMap), cp4)
      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(
        Header.modifierTypeId, blocks.take(5).map(block => block.header.id -> block.header.bytes).toMap), cp2)
      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(
        Header.modifierTypeId, blocks.take(5).map(block => block.header.id -> block.header.bytes).toMap), cp5)
      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(
        Header.modifierTypeId, blocks.take(2).map(block => block.header.id -> block.header.bytes).toMap), cp3)
      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(
        Header.modifierTypeId, blocks.take(2).map(block => block.header.id -> block.header.bytes).toMap), cp6)
      deliveryManager.underlyingActor.syncTracker.updatePeersPriorityStatus()
      val priorityResult1: (HistoryComparisonResult, PeerPriorityStatus, ConnectedPeer) =
        deliveryManager.underlyingActor.syncTracker.statuses(cp1.socketAddress.getAddress)
      val priorityResult2: (HistoryComparisonResult, PeerPriorityStatus, ConnectedPeer) =
        deliveryManager.underlyingActor.syncTracker.statuses(cp2.socketAddress.getAddress)
      val priorityResult3: (HistoryComparisonResult, PeerPriorityStatus, ConnectedPeer) =
        deliveryManager.underlyingActor.syncTracker.statuses(cp3.socketAddress.getAddress)
      val priorityResult4: (HistoryComparisonResult, PeerPriorityStatus, ConnectedPeer) =
        deliveryManager.underlyingActor.syncTracker.statuses(cp4.socketAddress.getAddress)
      val priorityResult5: (HistoryComparisonResult, PeerPriorityStatus, ConnectedPeer) =
        deliveryManager.underlyingActor.syncTracker.statuses(cp5.socketAddress.getAddress)
      val priorityResult6: (HistoryComparisonResult, PeerPriorityStatus, ConnectedPeer) =
        deliveryManager.underlyingActor.syncTracker.statuses(cp6.socketAddress.getAddress)

      assert(priorityResult1._3 == cp1)
      assert(priorityResult1._1 == Older)
      assert(priorityResult1._2 == 4)
      assert(priorityResult4._3 == cp4)
      assert(priorityResult4._1 == Older)
      assert(priorityResult4._2 == 4)

      assert(priorityResult2._3 == cp2)
      assert(priorityResult2._1 == Older)
      assert(priorityResult2._2 == 3)
      assert(priorityResult5._3 == cp5)
      assert(priorityResult5._1 == Older)
      assert(priorityResult5._2 == 3)

      assert(priorityResult3._3 == cp3)
      assert(priorityResult3._1 == Older)
      assert(priorityResult3._2 == 1)
      assert(priorityResult6._3 == cp6)
      assert(priorityResult6._1 == Older)
      assert(priorityResult6._2 == 1)
      deliveryManager.stop()
    }

    /**
      * This test simulates DeliveryManager behavior connected with updating node priority while receiving spam modifiers
      *
      * Test expected behavior is:
      * Send handshake to the Delivery Manager from cp1.
      * Receive unexpected modifiers from cp1.
      * cp1 priority must stay as InitialPriority.
      */
    "not increment modifiers which will be putted in spam collection" in {
      val (deliveryManager, cp1, _, _, _, _, _, blocks, _) = initialiseState
      deliveryManager ! HandshakedPeer(cp1)
      deliveryManager ! OtherNodeSyncingStatus(cp1, Older, None)
      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(
        Header.modifierTypeId, blocks.map(block => block.header.id -> block.header.bytes).toMap), cp1)
      deliveryManager.underlyingActor.syncTracker.updatePeersPriorityStatus()
      val priorityResultByPeer: (HistoryComparisonResult, PeerPriorityStatus, ConnectedPeer) =
        deliveryManager.underlyingActor.syncTracker.statuses(cp1.socketAddress.getAddress)

      assert(priorityResultByPeer._3 == cp1)
      assert(priorityResultByPeer._1 == Older)
      assert(priorityResultByPeer._2 == 1)
      deliveryManager.stop()
    }
  }
}