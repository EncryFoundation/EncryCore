package encry.network.DeliveryManagerTests

import java.net.InetSocketAddress
import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestKit, TestProbe}
import encry.consensus.History.{Fork, Older, Younger}
import encry.modifiers.InstanceFactory
import encry.modifiers.history.{Block, Header, Payload}
import encry.modifiers.mempool.Transaction
import encry.network.BasicMessagesRepo._
import encry.network.DeliveryManager
import encry.network.NetworkController.ReceivableMessages.DataFromPeer
import encry.network.NodeViewSynchronizer.ReceivableMessages.{HandshakedPeer, OtherNodeSyncingStatus, RequestFromLocal}
import encry.network.PeerConnectionHandler.{ConnectedPeer, Incoming}
import encry.settings.EncryAppSettings
import encry.utils.CoreTaggedTypes.ModifierId
import encry.view.EncryNodeViewHolder.DownloadRequest
import org.scalatest.{BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike}
import encry.network.DeliveryManagerTests.DMUtils._
import encry.view.history.EncrySyncInfo
import scala.collection.mutable.WrappedArray

class DeliveryManagerRequestModifiesSpec extends WordSpecLike with BeforeAndAfterAll
  with Matchers
  with InstanceFactory
  with OneInstancePerTest {

  implicit val system: ActorSystem = ActorSystem("SynchronousTestingSpec")
  val settings: EncryAppSettings = DummyEncryAppSettingsReader.read

  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  def initialiseState(isChainSynced: Boolean = true, isMining: Boolean = true): (TestActorRef[DeliveryManager],
    ConnectedPeer, ConnectedPeer, ConnectedPeer, List[Block], List[ModifierId], List[WrappedArray.ofByte]) = {
    val (deliveryManager, _) = initialiseDeliveryManager(isBlockChainSynced = isChainSynced, isMining = isMining, settings)
    val (_: InetSocketAddress, cp1: ConnectedPeer) = createPeer(9001, "172.16.13.10", settings)
    val (_: InetSocketAddress, cp2: ConnectedPeer) = createPeer(9002, "172.16.13.11", settings)
    val (_: InetSocketAddress, cp3: ConnectedPeer) = createPeer(9003, "172.16.13.12", settings)
    val blocks: List[Block] = generateBlocks(10, generateDummyHistory(settings))._2
    val headersIds: List[ModifierId] = blocks.map(_.header.id)
    val headersAsKey = headersIds.map(toKey)
    (deliveryManager, cp1, cp2, cp3, blocks, headersIds, headersAsKey)
  }

  "RequestModifies" should {
    "handle uniq modifiers from RequestFromLocal message correctly" in {
      val (deliveryManager, cp1, _, _, _, headersIds, headersAsKey) = initialiseState()
      deliveryManager ! HandshakedPeer(cp1)
      deliveryManager ! OtherNodeSyncingStatus(cp1, Older, None)
      deliveryManager ! RequestFromLocal(cp1, Header.modifierTypeId, headersIds)
      assert(deliveryManager.underlyingActor.expectedModifiers.getOrElse(cp1.socketAddress.getAddress, Map.empty)
        .keys.size == headersIds.size)
      assert(deliveryManager.underlyingActor.expectedModifiers.getOrElse(cp1.socketAddress.getAddress, Map.empty)
        .keys.forall(elem => headersAsKey.contains(elem)))
    }
    "not handle repeating modifiers from RequestFromLocal message" in {
      val (deliveryManager, cp1, _, _, _, headersIds, headersAsKey) = initialiseState()
      deliveryManager ! HandshakedPeer(cp1)
      deliveryManager ! OtherNodeSyncingStatus(cp1, Older, None)
      deliveryManager ! RequestFromLocal(cp1, Header.modifierTypeId, headersIds)
      deliveryManager ! RequestFromLocal(cp1, Header.modifierTypeId, headersIds)
      assert(deliveryManager.underlyingActor.expectedModifiers.getOrElse(cp1.socketAddress.getAddress, Map.empty)
        .keys.size == headersIds.size)
      assert(deliveryManager.underlyingActor.expectedModifiers.getOrElse(cp1.socketAddress.getAddress, Map.empty)
        .keys.forall(elem => headersAsKey.contains(elem)))
    }
    "Delivery Manager should handle received modifier which were requested correctly" in {
      val (deliveryManager, cp1, _, _, blocks, headersIds, headersAsKey) = initialiseState()
      deliveryManager ! HandshakedPeer(cp1)
      deliveryManager ! OtherNodeSyncingStatus(cp1, Older, None)
      deliveryManager ! RequestFromLocal(cp1, Header.modifierTypeId, headersIds)
      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(
        Header.modifierTypeId -> blocks.map(k => k.header.id -> Array.emptyByteArray).toMap), cp1)
      assert(deliveryManager.underlyingActor.expectedModifiers.getOrElse(cp1.socketAddress.getAddress, Map.empty)
        .keys.isEmpty)
      assert(deliveryManager.underlyingActor.receivedModifiers.size == blocks.size)
      assert(deliveryManager.underlyingActor.receivedModifiers.forall(elem => headersAsKey.contains(elem)))
      assert(deliveryManager.underlyingActor.headersForPriorityRequest.forall(x => headersAsKey.contains(x._1)))
    }
    "Delivery manager should not handle repeating modifiers" in {
      val (deliveryManager, cp1, _, _, blocks, headersIds, headersAsKey) = initialiseState()
      deliveryManager ! HandshakedPeer(cp1)
      deliveryManager ! OtherNodeSyncingStatus(cp1, Older, None)
      deliveryManager ! RequestFromLocal(cp1, Header.modifierTypeId, headersIds)
      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(
        Header.modifierTypeId -> blocks.map(k => k.header.id -> Array.emptyByteArray).toMap), cp1)
      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(
        Header.modifierTypeId -> blocks.map(k => k.header.id -> Array.emptyByteArray).toMap), cp1)
      assert(deliveryManager.underlyingActor.receivedModifiers.size == headersIds.size)
      assert(deliveryManager.underlyingActor.receivedModifiers.forall(elem => headersAsKey.contains(elem)))
      assert(deliveryManager.underlyingActor.headersForPriorityRequest.forall(x => headersAsKey.contains(x._1)))
    }
    "handle priority request for payload correctly" in {
      val (deliveryManager, cp1, _, _, blocks, headersIds, _) = initialiseState()
      deliveryManager ! HandshakedPeer(cp1)
      deliveryManager ! OtherNodeSyncingStatus(cp1, Older, None)
      deliveryManager ! RequestFromLocal(cp1, Header.modifierTypeId, headersIds)
      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(
        Header.modifierTypeId -> blocks.map(k => k.header.id -> Array.emptyByteArray).toMap), cp1)
      headersIds.foreach(id =>
        deliveryManager ! DownloadRequest(Payload.modifierTypeId, blocks.find(block =>
          block.id.sameElements(id)).get.payload.id, Some(id)))
      assert(deliveryManager.underlyingActor.expectedModifiers.getOrElse(cp1.socketAddress.getAddress, Map.empty)
        .size == blocks.size)
      assert(deliveryManager.underlyingActor.headersForPriorityRequest.isEmpty)
    }
    "choose correct peer in priority request" in {
      val (deliveryManager, _, _, _, blocks, _, _) = initialiseState()

      val address1 = new InetSocketAddress("123.123.123.123", 9001)
      val handler1: TestProbe = TestProbe()
      val cp1: ConnectedPeer = ConnectedPeer(address1, handler1.ref, Incoming,
        Handshake(protocolToBytes(settings.network.appVersion),
          "123.123.123.123", Some(address1), System.currentTimeMillis()))

      val address2 = new InetSocketAddress("123.123.123.124", 9001)
      val handler2: TestProbe = TestProbe()
      val cp2: ConnectedPeer = ConnectedPeer(address2, handler2.ref, Incoming,
        Handshake(protocolToBytes(settings.network.appVersion),
          "123.123.123.124", Some(address2), System.currentTimeMillis()))

      val address3 = new InetSocketAddress("123.123.123.125", 9001)
      val handler3: TestProbe = TestProbe()
      val cp3: ConnectedPeer = ConnectedPeer(address3, handler3.ref, Incoming,
        Handshake(protocolToBytes(settings.network.appVersion),
          "123.123.123.125", Some(address3), System.currentTimeMillis()))

      deliveryManager ! HandshakedPeer(cp1)
      deliveryManager ! OtherNodeSyncingStatus(cp1, Older, None)
      deliveryManager ! HandshakedPeer(cp2)
      deliveryManager ! OtherNodeSyncingStatus(cp2, Older, None)
      deliveryManager ! HandshakedPeer(cp3)
      deliveryManager ! OtherNodeSyncingStatus(cp3, Older, None)

      val header: Header = blocks.head.header

      deliveryManager ! RequestFromLocal(cp1, Header.modifierTypeId, Seq(header.id))
      deliveryManager ! RequestFromLocal(cp2, Header.modifierTypeId, Seq(header.id))
      deliveryManager ! RequestFromLocal(cp3, Header.modifierTypeId, Seq(header.id))

      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(Header.modifierTypeId, Map(header.id -> header.bytes)), cp1)
      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(Header.modifierTypeId, Map(header.id -> header.bytes)), cp2)
      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(Header.modifierTypeId, Map(header.id -> header.bytes)), cp3)

      deliveryManager ! DownloadRequest(Payload.modifierTypeId, header.payloadId, Some(header.id))

      handler1.expectMsgAllOf(
        RequestModifiersNetworkMessage(Header.modifierTypeId -> Seq(header.id)),
        RequestModifiersNetworkMessage(Payload.modifierTypeId -> Seq(header.payloadId))
      )
      handler2.expectMsgAllOf(RequestModifiersNetworkMessage(Header.modifierTypeId -> Seq(header.id)))
      handler3.expectMsgAllOf(RequestModifiersNetworkMessage(Header.modifierTypeId -> Seq(header.id)))
    }
    "not ask modifiers while block chain is not synced from Younger and Fork nodes" in {
      val (deliveryManager, _, _, _, blocks, _, _) = initialiseState(isChainSynced = false)

      val address2 = new InetSocketAddress("123.123.123.124", 9001)
      val handler2: TestProbe = TestProbe()
      val cp2: ConnectedPeer = ConnectedPeer(address2, handler2.ref, Incoming,
        Handshake(protocolToBytes(settings.network.appVersion),
          "123.123.123.124", Some(address2), System.currentTimeMillis()))

      val address3 = new InetSocketAddress("123.123.123.125", 9001)
      val handler3: TestProbe = TestProbe()
      val cp3: ConnectedPeer = ConnectedPeer(address3, handler3.ref, Incoming,
        Handshake(protocolToBytes(settings.network.appVersion),
          "123.123.123.125", Some(address3), System.currentTimeMillis()))

      deliveryManager ! HandshakedPeer(cp2)
      deliveryManager ! OtherNodeSyncingStatus(cp2, Fork, None)
      deliveryManager ! HandshakedPeer(cp3)
      deliveryManager ! OtherNodeSyncingStatus(cp3, Older, None)

      val header: Header = blocks.head.header

      deliveryManager ! RequestFromLocal(cp2, Header.modifierTypeId, Seq(header.id))
      deliveryManager ! RequestFromLocal(cp3, Header.modifierTypeId, Seq(header.id))

      handler2.expectNoMsg()
      handler3.expectMsgAllOf(RequestModifiersNetworkMessage(Header.modifierTypeId -> Seq(header.id)))
    }
    "not ask modifiers from peer which is not contained in status tracker" in {
      val (deliveryManager, _, _, _, blocks, _, _) = initialiseState()

      val address1 = new InetSocketAddress("123.123.123.123", 9001)
      val handler1: TestProbe = TestProbe()
      val cp1: ConnectedPeer = ConnectedPeer(address1, handler1.ref, Incoming,
        Handshake(protocolToBytes(settings.network.appVersion),
          "123.123.123.123", Some(address1), System.currentTimeMillis()))

      val address2 = new InetSocketAddress("123.123.123.124", 9001)
      val handler2: TestProbe = TestProbe()
      val cp2: ConnectedPeer = ConnectedPeer(address2, handler2.ref, Incoming,
        Handshake(protocolToBytes(settings.network.appVersion),
          "123.123.123.124", Some(address2), System.currentTimeMillis()))

      deliveryManager ! HandshakedPeer(cp2)
      deliveryManager ! OtherNodeSyncingStatus(cp2, Older, None)

      val header: Header = blocks.head.header

      deliveryManager ! RequestFromLocal(cp1, Header.modifierTypeId, Seq(header.id))
      deliveryManager ! RequestFromLocal(cp2, Header.modifierTypeId, Seq(header.id))

      handler1.expectNoMsg()
      handler2.expectMsgAllOf(RequestModifiersNetworkMessage(Header.modifierTypeId -> Seq(header.id)))
    }
    "not ask transactions while block chain is not synced" in {
      val (deliveryManager, _, _, _, _, _, _) = initialiseState(isChainSynced = false)
      val txs: Seq[Transaction] = genInvalidPaymentTxs(1)

      val address1 = new InetSocketAddress("123.123.123.123", 9001)
      val handler1: TestProbe = TestProbe()
      val cp1: ConnectedPeer = ConnectedPeer(address1, handler1.ref, Incoming,
        Handshake(protocolToBytes(settings.network.appVersion),
          "123.123.123.123", Some(address1), System.currentTimeMillis()))

      deliveryManager ! HandshakedPeer(cp1)
      deliveryManager ! OtherNodeSyncingStatus(cp1, Older, None)

      deliveryManager ! RequestFromLocal(cp1, Transaction.ModifierTypeId, txs.map(_.id))

      handler1.expectNoMsg()
    }
    "not ask transaction while node is not mining" in {
      val (deliveryManager, _, _, _, _, _, _) = initialiseState(isMining = false)
      val txs: Seq[Transaction] = genInvalidPaymentTxs(1)

      val address1 = new InetSocketAddress("123.123.123.123", 9001)
      val handler1: TestProbe = TestProbe()
      val cp1: ConnectedPeer = ConnectedPeer(address1, handler1.ref, Incoming,
        Handshake(protocolToBytes(settings.network.appVersion),
          "123.123.123.123", Some(address1), System.currentTimeMillis()))

      deliveryManager ! HandshakedPeer(cp1)
      deliveryManager ! OtherNodeSyncingStatus(cp1, Older, None)

      deliveryManager ! RequestFromLocal(cp1, Transaction.ModifierTypeId, txs.map(_.id))

      handler1.expectNoMsg()
    }
    "not re-ask modifiers which already have been received" in {
      val (deliveryManager, _, _, _, blocks, _, _) = initialiseState(isChainSynced = false)

      val address1 = new InetSocketAddress("123.123.123.123", 9001)
      val handler1: TestProbe = TestProbe()
      val cp1: ConnectedPeer = ConnectedPeer(address1, handler1.ref, Incoming,
        Handshake(protocolToBytes(settings.network.appVersion),
          "123.123.123.123", Some(address1), System.currentTimeMillis()))

      deliveryManager ! HandshakedPeer(cp1)
      deliveryManager ! OtherNodeSyncingStatus(cp1, Older, None)

      val header: Header = blocks.head.header

      deliveryManager ! RequestFromLocal(cp1, Header.modifierTypeId, Seq(header.id))

      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(Header.modifierTypeId, Map(header.id -> header.bytes)), cp1)

      handler1.expectMsgAllOf(
        RequestModifiersNetworkMessage(Header.modifierTypeId -> Seq(header.id)),
        SyncInfoNetworkMessage(EncrySyncInfo(List.empty))
      )

      deliveryManager ! RequestFromLocal(cp1, Header.modifierTypeId, Seq(header.id))

      handler1.expectNoMsg()

      assert(deliveryManager.underlyingActor.expectedModifiers.getOrElse(cp1.socketAddress.getAddress, Map.empty)
        .keys.isEmpty)
      assert(deliveryManager.underlyingActor.receivedModifiers.size == 1)
      assert(deliveryManager.underlyingActor.receivedModifiers.contains(toKey(header.id)))
    }
  }
}