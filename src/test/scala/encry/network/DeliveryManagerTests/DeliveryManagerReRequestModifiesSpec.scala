package encry.network.DeliveryManagerTests

import java.net.InetSocketAddress
import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestProbe}
import encry.consensus.HistoryConsensus
import encry.consensus.HistoryConsensus.Older
import encry.modifiers.InstanceFactory
import encry.network.DeliveryManager
import encry.network.DeliveryManagerTests.DMUtils._
import encry.network.NetworkController.ReceivableMessages.DataFromPeer
import encry.network.NodeViewSynchronizer.ReceivableMessages._
import encry.network.PeerConnectionHandler.{ConnectedPeer, Incoming}
import encry.network.PeersKeeper.UpdatedPeersCollection
import encry.network.PrioritiesCalculator.PeersPriorityStatus.PeersPriorityStatus.InitialPriority
import encry.network.PrioritiesCalculator.PeersPriorityStatus.PeersPriorityStatus
import encry.settings.EncryAppSettings
import encry.view.history.History
import org.encryfoundation.common.modifiers.history.{Block, Header, HeaderProtoSerializer}
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.network.BasicMessagesRepo.{Handshake, ModifiersNetworkMessage, RequestModifiersNetworkMessage}
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
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

  override def afterAll(): Unit = system.terminate()

  def initialiseState(isChainSynced: Boolean = true, isMining: Boolean = true): (TestActorRef[DeliveryManager],
    ConnectedPeer, ConnectedPeer, ConnectedPeer, List[Block], List[ModifierId], List[WrappedArray.ofByte], History) = {
    val (deliveryManager, history) =
      initialiseDeliveryManager(isBlockChainSynced = isChainSynced, isMining = isMining, settings)
    val (_: InetSocketAddress, cp1: ConnectedPeer) = createPeer(9001, "172.16.13.10", settings)
    val (_: InetSocketAddress, cp2: ConnectedPeer) = createPeer(9002, "172.16.13.11", settings)
    val (_: InetSocketAddress, cp3: ConnectedPeer) = createPeer(9003, "172.16.13.12", settings)
    val blocks: List[Block] = generateBlocks(10, generateDummyHistory(settings))._2
    val headersIds: List[ModifierId] = blocks.map(_.header.id)
    val headersAsKey = headersIds.map(toKey)
    (deliveryManager, cp1, cp2, cp3, blocks, headersIds, headersAsKey, history)
  }

  "ReRequestModifies" should {
    "re-ask necessary modifier several times (number of attempts from settings) and remove modifier from " +
      "expectedModifiers collection after all attempts will expire" in {
      val (deliveryManager, _, _, _, _, headersIds, _, _) = initialiseState()

      val address1 = new InetSocketAddress("123.123.123.123", 9001)
      val handler1: TestProbe = TestProbe()
      val cp1: ConnectedPeer = ConnectedPeer(address1, handler1.ref, Incoming,
        Handshake(protocolToBytes(settings.network.appVersion),
          "123.123.123.123", Some(address1), System.currentTimeMillis()))

      val updatedPeersCollection: Map[InetSocketAddress, (ConnectedPeer, HistoryConsensus.Older.type, PeersPriorityStatus)] =
        Map(address1 -> (cp1, Older, InitialPriority))

      deliveryManager ! UpdatedPeersCollection(updatedPeersCollection)

      val header: ModifierId = headersIds.head

      deliveryManager ! RequestFromLocal(cp1, Header.modifierTypeId, Seq(header))
      handler1.expectMsgAllOf(
        settings.network.deliveryTimeout * (settings.network.maxDeliveryChecks + 2),
        RequestModifiersNetworkMessage(Header.modifierTypeId -> Seq(header)),
        RequestModifiersNetworkMessage(Header.modifierTypeId -> Seq(header)),
        RequestModifiersNetworkMessage(Header.modifierTypeId -> Seq(header))
      )
      //this thread sleep is using for expecting modifier removal
      Thread.sleep(6000)

      assert(deliveryManager.underlyingActor.expectedModifiers.getOrElse(cp1.socketAddress, Map.empty).isEmpty)
      deliveryManager.stop()
    }
    "not re-ask unnecessary modifiers" in {
      val (deliveryManager, _, _, _, _, headersIds, _, _) = initialiseState()

      val address1 = new InetSocketAddress("123.123.123.123", 9001)
      val handler1: TestProbe = TestProbe()
      val cp1: ConnectedPeer = ConnectedPeer(address1, handler1.ref, Incoming,
        Handshake(protocolToBytes(settings.network.appVersion),
          "123.123.123.123", Some(address1), System.currentTimeMillis()))

      val updatedPeersCollection: Map[InetSocketAddress, (ConnectedPeer, HistoryConsensus.Older.type, PeersPriorityStatus)] =
        Map(address1 -> (cp1, Older, InitialPriority))

      deliveryManager ! UpdatedPeersCollection(updatedPeersCollection)

      val header: ModifierId = headersIds.head

      deliveryManager ! RequestFromLocal(cp1, Header.modifierTypeId, Seq(header))

      //await one re-ask
      handler1.expectMsgAllOf(
        settings.network.deliveryTimeout * (settings.network.maxDeliveryChecks + 2),
        RequestModifiersNetworkMessage(Header.modifierTypeId -> Seq(header)),
        RequestModifiersNetworkMessage(Header.modifierTypeId -> Seq(header))
      )

      val headerBytes: Array[Byte] = HeaderProtoSerializer.toProto(genHeader).toByteArray

      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(Header.modifierTypeId,
        Map(header -> headerBytes)), cp1)
      deliveryManager.stop()
    }
    "not re-ask modifiers which were applied to the history" in {
      val (deliveryManager, _, _, _, blocks, headerIds, _, history) = initialiseState()

      val address1 = new InetSocketAddress("123.123.123.123", 9001)
      val handler1: TestProbe = TestProbe()
      val cp1: ConnectedPeer = ConnectedPeer(address1, handler1.ref, Incoming,
        Handshake(protocolToBytes(settings.network.appVersion),
          "123.123.123.123", Some(address1), System.currentTimeMillis()))

      val updatedPeersCollection: Map[InetSocketAddress, (ConnectedPeer, HistoryConsensus.Older.type, PeersPriorityStatus)] =
        Map(address1 -> (cp1, Older, InitialPriority))

      deliveryManager ! UpdatedPeersCollection(updatedPeersCollection)

      deliveryManager ! RequestFromLocal(cp1, Header.modifierTypeId, Seq(headerIds.head))

      handler1.expectMsg(RequestModifiersNetworkMessage(Header.modifierTypeId -> Seq(headerIds.head)))

      val headerBytes: Array[Byte] = HeaderProtoSerializer.toProto(genHeader).toByteArray

      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(Header.modifierTypeId,
        Map(headerIds.head -> headerBytes)), cp1)

      val uHistory: History = history.append(blocks.head.header).right.get._1.reportModifierIsValid(blocks.head.header)

      deliveryManager ! UpdatedHistory(uHistory)

      deliveryManager ! SemanticallySuccessfulModifier(blocks.head.header)

      deliveryManager ! RequestFromLocal(cp1, Header.modifierTypeId, Seq(headerIds.head))

      assert(deliveryManager.underlyingActor.expectedModifiers
        .getOrElse(cp1.socketAddress, Map.empty).isEmpty)
      deliveryManager.stop()
    }
    "remove peer from expectedModifiers if expected modifiers collection from this peer is empty" in {
      val (deliveryManager, cp1, _, _, _, headerIds, _, _) = initialiseState()

      deliveryManager ! RequestFromLocal(cp1, Header.modifierTypeId, Seq(headerIds.head))
      //this thread sleep is using for expecting modifier removal
      Thread.sleep((settings.network.maxDeliveryChecks * settings.network.deliveryTimeout._1) * 1000)
      assert(deliveryManager.underlyingActor.expectedModifiers.getOrElse(cp1.socketAddress, Map.empty).isEmpty)
      assert(deliveryManager.underlyingActor.expectedModifiers
        .getOrElse(cp1.socketAddress, Map.empty) == Map.empty)
      deliveryManager.stop()
    }
    "not re-ask transactions" in {
      val (deliveryManager, _, _, _, _, _, _, _) = initialiseState()

      val address1 = new InetSocketAddress("123.123.123.123", 9001)
      val handler1: TestProbe = TestProbe()
      val cp1: ConnectedPeer = ConnectedPeer(address1, handler1.ref, Incoming,
        Handshake(protocolToBytes(settings.network.appVersion),
          "123.123.123.123", Some(address1), System.currentTimeMillis()))

      val updatedPeersCollection: Map[InetSocketAddress, (ConnectedPeer, HistoryConsensus.Older.type, PeersPriorityStatus)] =
        Map(address1 -> (cp1, Older, InitialPriority))

      deliveryManager ! UpdatedPeersCollection(updatedPeersCollection)

      val transactions: Seq[ModifierId] = genValidPaymentTxs(1).map(_.id)

      deliveryManager ! RequestFromLocal(cp1, Transaction.modifierTypeId, transactions)

      handler1.expectMsgAllOf(
        RequestModifiersNetworkMessage(Transaction.modifierTypeId -> transactions)
      )
      handler1.expectNoMsg(10.seconds)
      assert(deliveryManager.underlyingActor.expectedModifiers
        .getOrElse(cp1.socketAddress, Map.empty) == Map.empty)
      deliveryManager.stop()
    }
  }
}