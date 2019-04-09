package encry.network.DeliveryManagerTests

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestProbe}
import encry.consensus.History.{Equal, Older}
import encry.modifiers.InstanceFactory
import encry.modifiers.history.{Block, Header, Payload}
import encry.modifiers.mempool.Transaction
import encry.network.BasicMessagesRepo.{Handshake, ModifiersNetworkMessage, RequestModifiersNetworkMessage, SyncInfoNetworkMessage}
import encry.network.DeliveryManager
import encry.network.DeliveryManagerTests.DMUtils.{generateBlocks, initialiseDeliveryManager, toKey}
import encry.network.NetworkController.ReceivableMessages.DataFromPeer
import encry.network.NodeViewSynchronizer.ReceivableMessages._
import encry.network.PeerConnectionHandler.{ConnectedPeer, Incoming}
import encry.settings.EncryAppSettings
import encry.utils.CoreTaggedTypes.ModifierId
import encry.view.history.EncryHistory
import org.scalatest.{BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike}
import scala.concurrent.duration._
import scala.collection.mutable.WrappedArray

class DeliveryManagerReRequestModifiesSpec extends WordSpecLike
  with BeforeAndAfterAll
  with Matchers
  with InstanceFactory
  with OneInstancePerTest {

  implicit val system: ActorSystem = ActorSystem("SynchronousTestingSpec")
  val settings: EncryAppSettings = DummyEncryAppSettingsReader.read
  val threadPoolSize: Int = 1

  override def afterAll(): Unit = system.terminate()

  "ReRequestModifies" should {
//    val testProbeActor: TestProbe = TestProbe()
//    val initialState = initialiseDeliveryManager(isBlockChainSynced = false, isMining = true, settings)
//    val deliveryManager: TestActorRef[DeliveryManager] = initialState._1
//    val newPeer = new InetSocketAddress("172.16.13.10", 9001)
//    val peer: ConnectedPeer = ConnectedPeer(newPeer, testProbeActor.ref, Incoming,
//      Handshake(protocolToBytes(settings.network.appVersion), "peer", Some(newPeer), System.currentTimeMillis()))
//    val blocks: (EncryHistory, List[Block]) = generateBlocks(10, generateDummyHistory(settings))
//    val headerIds: List[ModifierId] = blocks._2.map(_.header.id)
//    val payloadIds: List[ModifierId] = blocks._2.map(_.header.payloadId)
//    deliveryManager ! HandshakedPeer(peer)
//    deliveryManager ! OtherNodeSyncingStatus(peer, Older, None)

//    "re-ask necessary modifier several times (number of attempts from settings) and remove modifier from " +
//      "expectedModifiers collection after all the attempts will expire" in {
//      deliveryManager ! RequestFromLocal(peer, Header.modifierTypeId, Seq(headerIds.head))
//      //settings.network.maxDeliveryChecks + (1) is using for expecting CheckDelivery which will trigger modifier removal
//      testProbeActor.receiveN(settings.network.maxDeliveryChecks + 1,
//        ((settings.network.maxDeliveryChecks + 1) * settings.network.deliveryTimeout._1).seconds)
//      //this thread sleep is using for expecting modifier removal
//      Thread.sleep((settings.network.deliveryTimeout._1 + 1) * 1000)
//      assert(deliveryManager.underlyingActor.expectedModifiers.getOrElse(newPeer.getAddress, Map.empty).isEmpty)
//    }
//    "don't re-ask unnecessary modifiers" in {
////      (0 to headerIds.size).foreach { _ =>
////        deliveryManager ! CheckDelivery(peer, Header.modifierTypeId, headerIds.head)
////        testProbeActor.expectNoMsg(0.5.seconds)
////        assert(deliveryManager.underlyingActor.expectedModifiers
////          .getOrElse(peer.socketAddress.getAddress, Map.empty).isEmpty)
////      }
//    }
//    "remove expired modifier from expectedModifiers collection" in {
//
//      val testProbeActor: TestProbe = TestProbe()
//      val initialState = initialiseDeliveryManager(isBlockChainSynced = false, isMining = true, settings)
//      val deliveryManager: TestActorRef[DeliveryManager] = initialState._1
//      val newPeer = new InetSocketAddress("172.16.13.10", 9001)
//      val peer: ConnectedPeer = ConnectedPeer(newPeer, testProbeActor.ref, Incoming,
//        Handshake(protocolToBytes(settings.network.appVersion), "peer", Some(newPeer), System.currentTimeMillis()))
//      val blocks: (EncryHistory, List[Block]) = generateBlocks(10, generateDummyHistory(settings))
//      val headerIds: List[ModifierId] = blocks._2.map(_.header.id)
//      val payloadIds: List[ModifierId] = blocks._2.map(_.header.payloadId)
//      deliveryManager ! HandshakedPeer(peer)
//      deliveryManager ! OtherNodeSyncingStatus(peer, Older, None)
//
//      val newPeer1 = new InetSocketAddress("172.16.13.20", 9001)
//      val peer1: ConnectedPeer = ConnectedPeer(newPeer1, testProbeActor.ref, Incoming,
//        Handshake(protocolToBytes(settings.network.appVersion), "peer1", Some(newPeer1), System.currentTimeMillis()))
//      deliveryManager ! HandshakedPeer(peer1)
//      deliveryManager ! OtherNodeSyncingStatus(peer1, Older, None)
//
//      val newPeer2 = new InetSocketAddress("172.16.13.30", 9001)
//      val peer2: ConnectedPeer = ConnectedPeer(newPeer2, testProbeActor.ref, Incoming,
//        Handshake(protocolToBytes(settings.network.appVersion), "peer2", Some(newPeer2), System.currentTimeMillis()))
//
//      deliveryManager ! HandshakedPeer(peer2)
//      deliveryManager ! OtherNodeSyncingStatus(peer2, Equal, None)
//
//      deliveryManager ! RequestFromLocal(peer, Header.modifierTypeId, headerIds.take(3))
//      //deliveryManager ! RequestFromLocal(peer1, Header.modifierTypeId, headerIds.takeRight(3))
//
//      blocks._2.take(3).foreach{block =>
//
//        deliveryManager ! DataFromPeer(ModifiersNetworkMessage(
//          Header.modifierTypeId -> Map(block.header.id -> block.header.bytes)), peer)
////
////        deliveryManager ! DataFromPeer(ModifiersNetworkMessage(
////          Header.modifierTypeId -> Map(block.header.id -> block.header.bytes)), peer1)
//      }
//
//      val updatedHistory: EncryHistory = blocks._2.take(3).foldLeft(initialState._2) { case (history, block) =>
//        history.append(block.header).get._1.reportModifierIsValid(block.header)
//      }
//
//      deliveryManager ! UpdatedHistory(updatedHistory)
//
//      Thread.sleep(100000)
//
////      deliveryManager ! RequestFromLocal(peer, Header.modifierTypeId, Seq(headerIds.head))
////      deliveryManager ! RequestFromLocal(peer1, Header.modifierTypeId, Seq(headerIds.head))
////      deliveryManager ! RequestFromLocal(peer2, Header.modifierTypeId, Seq(headerIds.head))
//
////      Thread.sleep(20000)
////      (0 to settings.network.maxDeliveryChecks).foreach(_ =>
////        deliveryManager ! CheckDelivery(peer, Header.modifierTypeId, headerIds.head))
////      assert(deliveryManager.underlyingActor.expectedModifiers
////        .getOrElse(peer.socketAddress.getAddress, Map.empty).isEmpty)
//    }
//    "correctly handle reRequest for modifier which number of tries has expired" in {
//
//      val newPeer1 = new InetSocketAddress("172.16.13.20", 9001)
//      val peer1: ConnectedPeer = ConnectedPeer(newPeer1, testProbeActor.ref, Incoming,
//        Handshake(protocolToBytes(settings.network.appVersion), "peer1", Some(newPeer1), System.currentTimeMillis()))
//      deliveryManager ! HandshakedPeer(peer1)
//      deliveryManager ! OtherNodeSyncingStatus(peer1, Older, None)
//
//      val newPeer2 = new InetSocketAddress("172.16.13.30", 9001)
//      val peer2: ConnectedPeer = ConnectedPeer(newPeer2, testProbeActor.ref, Incoming,
//        Handshake(protocolToBytes(settings.network.appVersion), "peer2", Some(newPeer2), System.currentTimeMillis()))
//      deliveryManager ! HandshakedPeer(peer2)
//      deliveryManager ! OtherNodeSyncingStatus(peer2, Older, None)
//
//      deliveryManager ! RequestFromLocal(peer, Header.modifierTypeId, Seq(headerIds.head))
//      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(
//        Header.modifierTypeId -> Map(blocks._2.head.header.id -> blocks._2.head.header.bytes)), peer)
//      val updatedHistory: EncryHistory =
//        initialState._2.append(blocks._2.head.header).get._1.reportModifierIsValid(blocks._2.head.header)
//      deliveryManager ! UpdatedHistory(updatedHistory)
//      deliveryManager ! CheckModifiersToDownload
////      testProbeActor.expectMsgAllOf(
////        RequestModifiersNetworkMessage(Header.modifierTypeId -> Seq(blocks._2.head.header.id)),
////        RequestModifiersNetworkMessage(Payload.modifierTypeId -> Seq(blocks._2.head.header.payloadId)),
////        SyncInfoNetworkMessage
////      )
//      Thread.sleep(25000)
////      //settings.network.maxDeliveryChecks + (1) is using for expecting CheckDelivery which will trigger modifier removal
////      testProbeActor.receiveN(settings.network.maxDeliveryChecks + 1,
////        ((settings.network.maxDeliveryChecks + 1) * settings.network.deliveryTimeout._1).seconds)
////      //this thread sleep is using for expecting modifier removal
////      Thread.sleep((settings.network.deliveryTimeout._1 + 1) * 1000)
////      assert(deliveryManager.underlyingActor.expectedModifiers.getOrElse(newPeer.getAddress, Map.empty).isEmpty)
//      deliveryManager ! CheckModifiersToDownload
//      Thread.sleep(25000)
//      deliveryManager ! CheckModifiersToDownload
//      Thread.sleep(25000)
//      deliveryManager ! CheckModifiersToDownload
//      Thread.sleep(25000)
//      //settings.network.maxDeliveryChecks + (1) is using for expecting CheckDelivery which will trigger modifier removal
//      testProbeActor.receiveN(settings.network.maxDeliveryChecks + 1,
//        ((settings.network.maxDeliveryChecks + 1) * settings.network.deliveryTimeout._1).seconds)
//      //this thread sleep is using for expecting modifier removal
//      Thread.sleep((settings.network.deliveryTimeout._1 + 1) * 1000)
//      assert(deliveryManager.underlyingActor.expectedModifiers.getOrElse(newPeer.getAddress, Map.empty).isEmpty)
//    }
//    "remove peer from expectedModifiers if expected modifiers collection from this peer is empty" in {
//      deliveryManager ! RequestFromLocal(peer, Header.modifierTypeId, Seq(headerIds.head))
////      (0 to settings.network.maxDeliveryChecks).foreach(_ =>
////        deliveryManager ! CheckDelivery(peer, Header.modifierTypeId, headerIds.head))
////      assert(deliveryManager.underlyingActor.expectedModifiers
////        .getOrElse(peer.socketAddress.getAddress, Map.empty) == Map.empty)
//    }
//    "remove reRequested transactions from expected modifiers if CheckDelivery " in {
//      val transactions: Seq[ModifierId] = genValidPaymentTxs(2).map(_.id)
//      val txsToKey: Seq[WrappedArray.ofByte] = transactions.map(toKey)
//      deliveryManager ! RequestFromLocal(peer, Transaction.ModifierTypeId, transactions)
////      deliveryManager ! CheckDelivery(peer, Transaction.ModifierTypeId, transactions.head)
//      assert(deliveryManager.underlyingActor.expectedModifiers
//        .getOrElse(peer.socketAddress.getAddress, Map.empty).size == 1)
//      assert(deliveryManager.underlyingActor.expectedModifiers
//        .getOrElse(peer.socketAddress.getAddress, Map.empty).forall(id => txsToKey.tail.contains(id._1)))
////      transactions.foreach(tx => deliveryManager ! CheckDelivery(peer, Transaction.ModifierTypeId, tx))
////      assert(deliveryManager.underlyingActor.expectedModifiers
////        .getOrElse(peer.socketAddress.getAddress, Map.empty) == Map.empty)
//    }
  }
}