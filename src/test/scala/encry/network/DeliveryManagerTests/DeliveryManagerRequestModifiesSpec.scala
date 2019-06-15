package encry.network.DeliveryManagerTests

import java.net.{InetAddress, InetSocketAddress}
import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestKit, TestProbe}
import encry.consensus.History
import encry.consensus.History.{Fork, Older, Younger}
import encry.modifiers.InstanceFactory
import encry.network.DeliveryManager
import encry.network.NetworkController.ReceivableMessages.DataFromPeer
import encry.network.NodeViewSynchronizer.ReceivableMessages.{ RequestFromLocal}
import encry.network.PeerConnectionHandler.{ConnectedPeer, Incoming}
import encry.settings.EncryAppSettings
import encry.view.NodeViewHolder.DownloadRequest
import org.scalatest.{BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike}
import encry.network.DeliveryManagerTests.DMUtils._
import encry.network.PeersKeeper.UpdatedPeersCollection
import encry.network.PrioritiesCalculator.PeersPriorityStatus.InitialPriority
import org.encryfoundation.common.modifiers.history.{Block, Header, HeaderProtoSerializer, Payload}
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.network.BasicMessagesRepo.{Handshake, ModifiersNetworkMessage, RequestModifiersNetworkMessage, SyncInfoNetworkMessage}
import org.encryfoundation.common.network.SyncInfo
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
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
      val updatedPeersCollection: Map[InetAddress, (ConnectedPeer, History.Older.type, InitialPriority)] =
        Map(cp1.socketAddress.getAddress -> (cp1, Older, InitialPriority()))

      deliveryManager ! UpdatedPeersCollection(updatedPeersCollection)

      deliveryManager ! RequestFromLocal(cp1, Header.modifierTypeId, headersIds)
      assert(deliveryManager.underlyingActor.expectedModifiers.getOrElse(cp1.socketAddress.getAddress, Map.empty)
        .keys.size == headersIds.size)
      assert(deliveryManager.underlyingActor.expectedModifiers.getOrElse(cp1.socketAddress.getAddress, Map.empty)
        .keys.forall(elem => headersAsKey.contains(elem)))
      deliveryManager.stop()
    }
    "not handle repeating modifiers from RequestFromLocal message" in {
      val (deliveryManager, cp1, _, _, _, headersIds, headersAsKey) = initialiseState()
      val updatedPeersCollection: Map[InetAddress, (ConnectedPeer, History.Older.type, InitialPriority)] =
        Map(cp1.socketAddress.getAddress -> (cp1, Older, InitialPriority()))

      deliveryManager ! UpdatedPeersCollection(updatedPeersCollection)
      deliveryManager ! RequestFromLocal(cp1, Header.modifierTypeId, headersIds)
      deliveryManager ! RequestFromLocal(cp1, Header.modifierTypeId, headersIds)
      assert(deliveryManager.underlyingActor.expectedModifiers.getOrElse(cp1.socketAddress.getAddress, Map.empty)
        .keys.size == headersIds.size)
      assert(deliveryManager.underlyingActor.expectedModifiers.getOrElse(cp1.socketAddress.getAddress, Map.empty)
        .keys.forall(elem => headersAsKey.contains(elem)))
      deliveryManager.stop()
    }
    "Delivery Manager should handle received modifier which were requested correctly" in {
      val (deliveryManager, cp1, _, _, blocks, headersIds, headersAsKey) = initialiseState()
      val updatedPeersCollection: Map[InetAddress, (ConnectedPeer, History.Older.type, InitialPriority)] =
        Map(cp1.socketAddress.getAddress -> (cp1, Older, InitialPriority()))

      deliveryManager ! UpdatedPeersCollection(updatedPeersCollection)
      deliveryManager ! RequestFromLocal(cp1, Header.modifierTypeId, headersIds)
      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(
        Header.modifierTypeId -> blocks.map(k => k.header.id -> Array.emptyByteArray).toMap), cp1)
      assert(deliveryManager.underlyingActor.expectedModifiers.getOrElse(cp1.socketAddress.getAddress, Map.empty)
        .keys.isEmpty)
      assert(deliveryManager.underlyingActor.receivedModifiers.size == blocks.size)
      assert(deliveryManager.underlyingActor.receivedModifiers.forall(elem => headersAsKey.contains(elem)))
      assert(deliveryManager.underlyingActor.headersForPriorityRequest.forall(x => headersAsKey.contains(x._1)))
      deliveryManager.stop()
    }
    "Delivery manager should not handle repeating modifiers" in {
      val (deliveryManager, cp1, _, _, blocks, headersIds, headersAsKey) = initialiseState()
      val updatedPeersCollection: Map[InetAddress, (ConnectedPeer, History.Older.type, InitialPriority)] =
        Map(cp1.socketAddress.getAddress -> (cp1, Older, InitialPriority()))

      deliveryManager ! UpdatedPeersCollection(updatedPeersCollection)
      deliveryManager ! RequestFromLocal(cp1, Header.modifierTypeId, headersIds)
      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(
        Header.modifierTypeId -> blocks.map(k => k.header.id -> Array.emptyByteArray).toMap), cp1)
      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(
        Header.modifierTypeId -> blocks.map(k => k.header.id -> Array.emptyByteArray).toMap), cp1)
      assert(deliveryManager.underlyingActor.receivedModifiers.size == headersIds.size)
      assert(deliveryManager.underlyingActor.receivedModifiers.forall(elem => headersAsKey.contains(elem)))
      assert(deliveryManager.underlyingActor.headersForPriorityRequest.forall(x => headersAsKey.contains(x._1)))
      deliveryManager.stop()
    }
    "handle priority request for payload correctly" in {
      val (deliveryManager, cp1, _, _, blocks, headersIds, _) = initialiseState()
      val updatedPeersCollection: Map[InetAddress, (ConnectedPeer, History.Older.type, InitialPriority)] =
        Map(cp1.socketAddress.getAddress -> (cp1, Older, InitialPriority()))

      deliveryManager ! UpdatedPeersCollection(updatedPeersCollection)
      deliveryManager ! RequestFromLocal(cp1, Header.modifierTypeId, headersIds)
      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(
        Header.modifierTypeId -> blocks.map(k => k.header.id -> Array.emptyByteArray).toMap), cp1)
      headersIds.foreach(id =>
        deliveryManager ! DownloadRequest(Payload.modifierTypeId, blocks.find(block =>
          block.id.sameElements(id)).get.payload.id, Some(id)))
      assert(deliveryManager.underlyingActor.expectedModifiers.getOrElse(cp1.socketAddress.getAddress, Map.empty)
        .size == blocks.size)
      assert(deliveryManager.underlyingActor.headersForPriorityRequest.isEmpty)
      deliveryManager.stop()
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

      val updatedPeersCollection: Map[InetAddress, (ConnectedPeer, History.Older.type, InitialPriority)] =
        Map(
          address1.getAddress -> (cp1, Older, InitialPriority()),
          address2.getAddress -> (cp2, Older, InitialPriority()),
          address3.getAddress -> (cp3, Older, InitialPriority())
        )

      deliveryManager ! UpdatedPeersCollection(updatedPeersCollection)

      val header: Header = blocks.head.header

      deliveryManager ! RequestFromLocal(cp1, Header.modifierTypeId, Seq(header.id))
      deliveryManager ! RequestFromLocal(cp2, Header.modifierTypeId, Seq(header.id))
      deliveryManager ! RequestFromLocal(cp3, Header.modifierTypeId, Seq(header.id))

      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(Header.modifierTypeId, Map(header.id -> header.bytes)), cp1)
      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(Header.modifierTypeId, Map(header.id -> header.bytes)), cp2)
      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(Header.modifierTypeId, Map(header.id -> header.bytes)), cp3)

      deliveryManager ! DownloadRequest(Payload.modifierTypeId, header.payloadId, Some(header.id))

      handler1.expectMsgAnyOf(
        RequestModifiersNetworkMessage(Header.modifierTypeId -> Seq(header.id)),
        RequestModifiersNetworkMessage(Payload.modifierTypeId -> Seq(header.payloadId)),
        SyncInfoNetworkMessage(SyncInfo(List()))
      )

      handler2.expectMsgAllOf(RequestModifiersNetworkMessage(Header.modifierTypeId -> Seq(header.id)))
      handler3.expectMsgAllOf(RequestModifiersNetworkMessage(Header.modifierTypeId -> Seq(header.id)))
      deliveryManager.stop()
    }
    "not ask modifiers while block chain is not synced from Younger nodes" in {
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

      val updatedPeersCollection =
        Map(
          address2.getAddress -> (cp2, Younger, InitialPriority()),
          address3.getAddress -> (cp3, Fork, InitialPriority())
        )

      deliveryManager ! UpdatedPeersCollection(updatedPeersCollection)


      val header: Header = blocks.head.header

      deliveryManager ! RequestFromLocal(cp2, Header.modifierTypeId, Seq(header.id))
      deliveryManager ! RequestFromLocal(cp3, Header.modifierTypeId, Seq(header.id))

      handler2.expectNoMsg()
      handler3.expectMsgAllOf(RequestModifiersNetworkMessage(Header.modifierTypeId -> Seq(header.id)))
      deliveryManager.stop()
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

      val updatedPeersCollection: Map[InetAddress, (ConnectedPeer, History.Older.type, InitialPriority)] =
        Map(address2.getAddress -> (cp2, Older, InitialPriority()))

      deliveryManager ! UpdatedPeersCollection(updatedPeersCollection)

      val header: Header = blocks.head.header

      deliveryManager ! RequestFromLocal(cp1, Header.modifierTypeId, Seq(header.id))
      deliveryManager ! RequestFromLocal(cp2, Header.modifierTypeId, Seq(header.id))

      handler1.expectNoMsg()
      handler2.expectMsgAllOf(RequestModifiersNetworkMessage(Header.modifierTypeId -> Seq(header.id)))
      deliveryManager.stop()
    }
    "not ask transactions while block chain is not synced" in {
      val (deliveryManager, _, _, _, _, _, _) = initialiseState(isChainSynced = false)
      val txs: Seq[Transaction] = genInvalidPaymentTxs(1)

      val address1 = new InetSocketAddress("123.123.123.123", 9001)
      val handler1: TestProbe = TestProbe()
      val cp1: ConnectedPeer = ConnectedPeer(address1, handler1.ref, Incoming,
        Handshake(protocolToBytes(settings.network.appVersion),
          "123.123.123.123", Some(address1), System.currentTimeMillis()))

      val updatedPeersCollection: Map[InetAddress, (ConnectedPeer, History.Older.type, InitialPriority)] =
        Map(address1.getAddress -> (cp1, Older, InitialPriority()))

      deliveryManager ! UpdatedPeersCollection(updatedPeersCollection)

      deliveryManager ! RequestFromLocal(cp1, Transaction.modifierTypeId, txs.map(_.id))

      handler1.expectNoMsg()
      deliveryManager.stop()
    }
    "not ask transaction while node is not mining" in {
      val (deliveryManager, _, _, _, _, _, _) = initialiseState(isMining = false)
      val txs: Seq[Transaction] = genInvalidPaymentTxs(1)

      val address1 = new InetSocketAddress("123.123.123.123", 9001)
      val handler1: TestProbe = TestProbe()
      val cp1: ConnectedPeer = ConnectedPeer(address1, handler1.ref, Incoming,
        Handshake(protocolToBytes(settings.network.appVersion),
          "123.123.123.123", Some(address1), System.currentTimeMillis()))

      val updatedPeersCollection: Map[InetAddress, (ConnectedPeer, History.Older.type, InitialPriority)] =
        Map(address1.getAddress -> (cp1, Older, InitialPriority()))

      deliveryManager ! UpdatedPeersCollection(updatedPeersCollection)

      deliveryManager ! RequestFromLocal(cp1, Transaction.modifierTypeId, txs.map(_.id))

      handler1.expectNoMsg()
      deliveryManager.stop()
    }
    "not re-ask modifiers which already have been received" in {
      val (deliveryManager, _, _, _, blocks, _, _) = initialiseState(isChainSynced = false)

      val address1 = new InetSocketAddress("123.123.123.123", 9001)
      val handler1: TestProbe = TestProbe()
      val cp1: ConnectedPeer = ConnectedPeer(address1, handler1.ref, Incoming,
        Handshake(protocolToBytes(settings.network.appVersion),
          "123.123.123.123", Some(address1), System.currentTimeMillis()))

      val updatedPeersCollection: Map[InetAddress, (ConnectedPeer, History.Older.type, InitialPriority)] =
        Map(address1.getAddress -> (cp1, Older, InitialPriority()))

      deliveryManager ! UpdatedPeersCollection(updatedPeersCollection)

      val header: Header = blocks.head.header

      deliveryManager ! RequestFromLocal(cp1, Header.modifierTypeId, Seq(header.id))

      val headerBytes: Array[Byte] = HeaderProtoSerializer.toProto(header).toByteArray

      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(Header.modifierTypeId, Map(header.id -> headerBytes)), cp1)

      handler1.expectMsgAllOf(
        RequestModifiersNetworkMessage(Header.modifierTypeId -> Seq(header.id)),
      )

      deliveryManager ! RequestFromLocal(cp1, Header.modifierTypeId, Seq(header.id))

      handler1.expectNoMsg()

      assert(deliveryManager.underlyingActor.expectedModifiers.getOrElse(cp1.socketAddress.getAddress, Map.empty)
        .keys.isEmpty)
      assert(deliveryManager.underlyingActor.receivedModifiers.size == 1)
      assert(deliveryManager.underlyingActor.receivedModifiers.contains(toKey(header.id)))
      deliveryManager.stop()
    }
  }
}