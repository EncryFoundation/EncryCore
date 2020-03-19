package encry.network.DeliveryManagerTests

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestKit, TestProbe}
import encry.consensus.HistoryConsensus
import encry.consensus.HistoryConsensus.{Fork, Older, Younger}
import encry.modifiers.InstanceFactory
import encry.network.DM.RequestSent
import encry.network.{DM, DeliveryManager}
import encry.network.NetworkController.ReceivableMessages.DataFromPeer
import encry.network.PeerConnectionHandler.{ConnectedPeer, Incoming}
import encry.settings.TestNetSettings
import org.scalatest.{BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike}
import encry.network.DeliveryManagerTests.DMUtils._
import encry.network.Messages.MessageToNetwork.RequestFromLocal
import encry.network.PeersKeeper.UpdatedPeersCollection
import encry.network.PrioritiesCalculator.PeersPriorityStatus.PeersPriorityStatus
import encry.network.PrioritiesCalculator.PeersPriorityStatus.PeersPriorityStatus._
import org.encryfoundation.common.modifiers.history.{Block, Header, HeaderProtoSerializer, Payload}
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.network.BasicMessagesRepo.{Handshake, ModifiersNetworkMessage, RequestModifiersNetworkMessage, SyncInfoNetworkMessage}
import org.encryfoundation.common.network.SyncInfo
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId

import scala.collection.mutable.WrappedArray

class DeliveryManagerRequestModifiesSpec extends WordSpecLike with BeforeAndAfterAll
  with Matchers
  with InstanceFactory
  with OneInstancePerTest
  with TestNetSettings {

  implicit val system: ActorSystem = ActorSystem("SynchronousTestingSpec")

  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  def initialiseState(isChainSynced: Boolean = true, isMining: Boolean = true): (TestProbe, TestActorRef[DM],
    ConnectedPeer, ConnectedPeer, ConnectedPeer, List[Block], List[ModifierId], List[WrappedArray.ofByte]) = {
    val (networkRouter, deliveryManager, _) = initialiseDeliveryManager(isBlockChainSynced = isChainSynced, isMining = isMining, testNetSettings)
    val (_: InetSocketAddress, cp1: ConnectedPeer) = createPeer(9001, "172.16.13.10", testNetSettings)
    val (_: InetSocketAddress, cp2: ConnectedPeer) = createPeer(9002, "172.16.13.11", testNetSettings)
    val (_: InetSocketAddress, cp3: ConnectedPeer) = createPeer(9003, "172.16.13.12", testNetSettings)
    val blocks: List[Block] = generateBlocks(10, generateDummyHistory(testNetSettings))._2
    val headersIds: List[ModifierId] = blocks.map(_.header.id)
    val headersAsKey = headersIds.map(toKey)
    (networkRouter, deliveryManager, cp1, cp2, cp3, blocks, headersIds, headersAsKey)
  }

  "RequestModifies" should {
    "handle uniq modifiers from RequestFromLocal message correctly" in {
      val (_, deliveryManager, cp1, _, _, _, headersIds, headersAsKey) = initialiseState()
      val updatedPeersCollection: Map[InetSocketAddress, (ConnectedPeer, HistoryConsensus.Older.type, PeersPriorityStatus)] =
        Map(cp1.socketAddress -> (cp1, Older, InitialPriority))

      headersIds.foreach(id => deliveryManager ! RequestSent(cp1.socketAddress, Header.modifierTypeId, id))
      assert(deliveryManager.underlyingActor.expectedModifiers.size == headersIds.size)
      assert(deliveryManager.underlyingActor.expectedModifiers.forall(elem => headersAsKey.contains(elem)))
      deliveryManager.stop()
    }
    "not handle repeating modifiers from RequestFromLocal message" in {
      val (_, deliveryManager, cp1, _, _, _, headersIds, headersAsKey) = initialiseState()
      val updatedPeersCollection: Map[InetSocketAddress, (ConnectedPeer, HistoryConsensus.Older.type, PeersPriorityStatus)] =
        Map(cp1.socketAddress -> (cp1, Older, InitialPriority))

      headersIds.foreach(id => deliveryManager ! RequestSent(cp1.socketAddress, Header.modifierTypeId, id))
      headersIds.foreach(id => deliveryManager ! RequestSent(cp1.socketAddress, Header.modifierTypeId, id))
      assert(deliveryManager.underlyingActor.expectedModifiers.size == headersIds.size)
      assert(deliveryManager.underlyingActor.expectedModifiers.forall(elem => headersAsKey.contains(elem)))
      deliveryManager.stop()
    }
    "Delivery Manager should handle received modifier which were requested correctly" in {
      val (_, deliveryManager, cp1, _, _, blocks, headersIds, headersAsKey) = initialiseState()
      val updatedPeersCollection: Map[InetSocketAddress, (ConnectedPeer, HistoryConsensus.Older.type, PeersPriorityStatus)] =
        Map(cp1.socketAddress -> (cp1, Older, InitialPriority))

      headersIds.foreach(id => deliveryManager ! RequestSent(cp1.socketAddress, Header.modifierTypeId, id))
      val headerBytes: Array[Byte] = HeaderProtoSerializer.toProto(genHeader).toByteArray
      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(
        Header.modifierTypeId -> blocks.map(k => k.header.id -> headerBytes).toMap), cp1.socketAddress)
      assert(deliveryManager.underlyingActor.expectedModifiers.isEmpty)
      assert(deliveryManager.underlyingActor.receivedModifiers.size == blocks.size)
      assert(deliveryManager.underlyingActor.receivedModifiers.forall(elem => headersAsKey.contains(elem)))
      deliveryManager.stop()
    }
    "Delivery manager should not handle repeating modifiers" in {
      val (_, deliveryManager, cp1, _, _, blocks, headersIds, headersAsKey) = initialiseState()
      val updatedPeersCollection: Map[InetSocketAddress, (ConnectedPeer, HistoryConsensus.Older.type, PeersPriorityStatus)] =
        Map(cp1.socketAddress -> (cp1, Older, InitialPriority))

      headersIds.foreach(id => deliveryManager ! RequestSent(cp1.socketAddress, Header.modifierTypeId, id))
      val headerBytes: Array[Byte] = HeaderProtoSerializer.toProto(genHeader).toByteArray
      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(
        Header.modifierTypeId -> blocks.map(k => k.header.id -> headerBytes).toMap), cp1.socketAddress)
      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(
        Header.modifierTypeId -> blocks.map(k => k.header.id -> headerBytes).toMap), cp1.socketAddress)
      assert(deliveryManager.underlyingActor.receivedModifiers.size == headersIds.size)
      assert(deliveryManager.underlyingActor.receivedModifiers.forall(elem => headersAsKey.contains(elem)))
      deliveryManager.stop()
    }
    "handle priority request for payload correctly" in {
      val (_, deliveryManager, cp1, _, _, blocks, headersIds, _) = initialiseState()
      val updatedPeersCollection: Map[InetSocketAddress, (ConnectedPeer, HistoryConsensus.Older.type, PeersPriorityStatus)] =
        Map(cp1.socketAddress -> (cp1, Older, InitialPriority))

      headersIds.foreach(id => deliveryManager ! RequestSent(cp1.socketAddress, Header.modifierTypeId, id))
      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(
        Header.modifierTypeId -> blocks.map(k => k.header.id -> Array.emptyByteArray).toMap), cp1.socketAddress)
      headersIds.foreach(id =>
        deliveryManager ! RequestSent(cp1.socketAddress, Payload.modifierTypeId, blocks.find(block =>
          block.id.sameElements(id)).get.payload.id))
      assert(deliveryManager.underlyingActor.expectedModifiers.size == blocks.size)
      deliveryManager.stop()
    }
    //todo: reinit
//    "choose correct peer in priority request" in {
//      val (deliveryManager, _, _, _, blocks, _, _) = initialiseState()
//
//      val address1 = new InetSocketAddress("123.123.123.123", 9001)
//      val handler1: TestProbe = TestProbe()
//      val cp1: ConnectedPeer = ConnectedPeer(address1, handler1.ref, Incoming,
//        Handshake(protocolToBytes(testNetSettings.network.appVersion),
//          "123.123.123.123", Some(address1), System.currentTimeMillis()))
//
//      val address2 = new InetSocketAddress("123.123.123.124", 9001)
//      val handler2: TestProbe = TestProbe()
//      val cp2: ConnectedPeer = ConnectedPeer(address2, handler2.ref, Incoming,
//        Handshake(protocolToBytes(testNetSettings.network.appVersion),
//          "123.123.123.124", Some(address2), System.currentTimeMillis()))
//
//      val address3 = new InetSocketAddress("123.123.123.125", 9001)
//      val handler3: TestProbe = TestProbe()
//      val cp3: ConnectedPeer = ConnectedPeer(address3, handler3.ref, Incoming,
//        Handshake(protocolToBytes(testNetSettings.network.appVersion),
//          "123.123.123.125", Some(address3), System.currentTimeMillis()))
//
//      val updatedPeersCollection: Map[InetSocketAddress, (ConnectedPeer, HistoryConsensus.Older.type, PeersPriorityStatus)] =
//        Map(
//          address1 -> (cp1, Older, InitialPriority),
//          address2 -> (cp2, Older, InitialPriority),
//          address3 -> (cp3, Older, InitialPriority)
//        )
//
//
//
//      val header: Header = blocks.head.header
//
//      deliveryManager ! RequestSent(cp1.socketAddress, Header.modifierTypeId, header.id)
//      deliveryManager ! RequestSent(cp2.socketAddress, Header.modifierTypeId, header.id)
//      deliveryManager ! RequestSent(cp3.socketAddress, Header.modifierTypeId, header.id)
//
//      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(Header.modifierTypeId, Map(header.id -> header.bytes)), cp1.socketAddress)
//      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(Header.modifierTypeId, Map(header.id -> header.bytes)), cp2.socketAddress)
//      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(Header.modifierTypeId, Map(header.id -> header.bytes)), cp3.socketAddress)
//
//      deliveryManager ! RequestSent(cp1.socketAddress, Payload.modifierTypeId, header.payloadId)
//
//      handler1.expectMsgAnyOf(
//        RequestModifiersNetworkMessage(Header.modifierTypeId -> Seq(header.id)),
//        RequestModifiersNetworkMessage(Payload.modifierTypeId -> Seq(header.payloadId)),
//        SyncInfoNetworkMessage(SyncInfo(List()))
//      )
//
//      handler2.expectMsgAllOf(RequestModifiersNetworkMessage(Header.modifierTypeId -> Seq(header.id)))
//      handler3.expectMsgAllOf(RequestModifiersNetworkMessage(Header.modifierTypeId -> Seq(header.id)))
//      deliveryManager.stop()
//    }
    //todo: reinit
//    "not ask modifiers from peer which is not contained in status tracker" in {
//      val (deliveryManager, _, _, _, blocks, _, _) = initialiseState()
//
//      val address1 = new InetSocketAddress("123.123.123.123", 9001)
//      val handler1: TestProbe = TestProbe()
//      val cp1: ConnectedPeer = ConnectedPeer(address1, handler1.ref, Incoming,
//        Handshake(protocolToBytes(testNetSettings.network.appVersion),
//          "123.123.123.123", Some(address1), System.currentTimeMillis()))
//
//      val address2 = new InetSocketAddress("123.123.123.124", 9001)
//      val handler2: TestProbe = TestProbe()
//      val cp2: ConnectedPeer = ConnectedPeer(address2, handler2.ref, Incoming,
//        Handshake(protocolToBytes(testNetSettings.network.appVersion),
//          "123.123.123.124", Some(address2), System.currentTimeMillis()))
//
//      val updatedPeersCollection: Map[InetSocketAddress, (ConnectedPeer, HistoryConsensus.Older.type, PeersPriorityStatus)] =
//        Map(address2 -> (cp2, Older, InitialPriority))
//
//      val header: Header = blocks.head.header
//
//      deliveryManager ! RequestSent(cp1.socketAddress, Header.modifierTypeId, header.id)
//      deliveryManager ! RequestSent(cp2.socketAddress, Header.modifierTypeId, header.id)
//
//      handler1.expectNoMsg()
//      handler2.expectMsgAllOf(RequestModifiersNetworkMessage(Header.modifierTypeId -> Seq(header.id)))
//      deliveryManager.stop()
//    }
    "not ask transactions while block chain is not synced" in {
      val (_, deliveryManager, _, _, _, _, _, _) = initialiseState(isChainSynced = false)
      val txs: Seq[Transaction] = genInvalidPaymentTxs(1)

      val address1 = new InetSocketAddress("123.123.123.123", 9001)
      val handler1: TestProbe = TestProbe()
      val cp1: ConnectedPeer = ConnectedPeer(address1, handler1.ref, Incoming,
        Handshake(protocolToBytes(testNetSettings.network.appVersion),
          "123.123.123.123", Some(address1), System.currentTimeMillis()))

      val updatedPeersCollection: Map[InetSocketAddress, (ConnectedPeer, HistoryConsensus.Older.type, PeersPriorityStatus)] =
        Map(address1 -> (cp1, Older, InitialPriority))

      

      txs.foreach(tx => deliveryManager ! RequestSent(cp1.socketAddress, Transaction.modifierTypeId, tx.id))

      handler1.expectNoMsg()
      deliveryManager.stop()
    }
    "not ask transaction while node is not mining" in {
      val (_, deliveryManager, _, _, _, _, _, _) = initialiseState(isMining = false)
      val txs: Seq[Transaction] = genInvalidPaymentTxs(1)

      val address1 = new InetSocketAddress("123.123.123.123", 9001)
      val handler1: TestProbe = TestProbe()
      val cp1: ConnectedPeer = ConnectedPeer(address1, handler1.ref, Incoming,
        Handshake(protocolToBytes(testNetSettings.network.appVersion),
          "123.123.123.123", Some(address1), System.currentTimeMillis()))

      val updatedPeersCollection: Map[InetSocketAddress, (ConnectedPeer, HistoryConsensus.Older.type, PeersPriorityStatus)] =
        Map(address1 -> (cp1, Older, InitialPriority))

      

      txs.foreach(tx => deliveryManager ! RequestSent(cp1.socketAddress, Transaction.modifierTypeId, tx.id))

      handler1.expectNoMsg()
      deliveryManager.stop()
    }
    "not re-ask modifiers which already have been received" in {
      val (_, deliveryManager, _, _, _, blocks, _, _) = initialiseState(isChainSynced = false)

      val address1 = new InetSocketAddress("123.123.123.123", 9001)
      val handler1: TestProbe = TestProbe()
      val cp1: ConnectedPeer = ConnectedPeer(address1, handler1.ref, Incoming,
        Handshake(protocolToBytes(testNetSettings.network.appVersion),
          "123.123.123.123", Some(address1), System.currentTimeMillis()))

      val updatedPeersCollection: Map[InetSocketAddress, (ConnectedPeer, HistoryConsensus.Older.type, PeersPriorityStatus)] =
        Map(address1 -> (cp1, Older, InitialPriority))

      

      val header: Header = blocks.head.header

      deliveryManager ! RequestSent(cp1.socketAddress, Header.modifierTypeId, header.id)

      val headerBytes: Array[Byte] = HeaderProtoSerializer.toProto(header).toByteArray

      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(Header.modifierTypeId, Map(header.id -> headerBytes)), cp1.socketAddress)

      deliveryManager ! RequestSent(cp1.socketAddress, Header.modifierTypeId, header.id)

      assert(deliveryManager.underlyingActor.expectedModifiers.isEmpty)
      assert(deliveryManager.underlyingActor.receivedModifiers.size == 1)
      assert(deliveryManager.underlyingActor.receivedModifiers.contains(toKey(header.id)))
      deliveryManager.stop()
    }
  }
}