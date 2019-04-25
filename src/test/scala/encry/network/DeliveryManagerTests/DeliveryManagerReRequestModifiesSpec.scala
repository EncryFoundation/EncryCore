package encry.network.DeliveryManagerTests

import java.net.InetSocketAddress
import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestProbe}
import encry.consensus.History.Older
import encry.modifiers.InstanceFactory
import encry.modifiers.history.{Block, Header}
import encry.modifiers.mempool.Transaction
import encry.network.BasicMessagesRepo._
import encry.network.DeliveryManager
import encry.network.DeliveryManagerTests.DMUtils._
import encry.network.NetworkController.ReceivableMessages.DataFromPeer
import encry.network.NodeViewSynchronizer.ReceivableMessages._
import encry.network.PeerConnectionHandler.{ConnectedPeer, Incoming}
import encry.settings.EncryAppSettings
import encry.utils.CoreTaggedTypes.ModifierId
import encry.view.history.{EncryHistory, EncrySyncInfo}
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
    ConnectedPeer, ConnectedPeer, ConnectedPeer, List[Block], List[ModifierId], List[WrappedArray.ofByte], EncryHistory) = {
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

      deliveryManager ! HandshakedPeer(cp1)
      deliveryManager ! OtherNodeSyncingStatus(cp1, Older, None)

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

      assert(deliveryManager.underlyingActor.expectedModifiers.getOrElse(cp1.socketAddress.getAddress, Map.empty).isEmpty)
    }
    "not re-ask unnecessary modifiers" in {
      val (deliveryManager, _, _, _, _, headersIds, _, _) = initialiseState()

      val address1 = new InetSocketAddress("123.123.123.123", 9001)
      val handler1: TestProbe = TestProbe()
      val cp1: ConnectedPeer = ConnectedPeer(address1, handler1.ref, Incoming,
        Handshake(protocolToBytes(settings.network.appVersion),
          "123.123.123.123", Some(address1), System.currentTimeMillis()))

      deliveryManager ! HandshakedPeer(cp1)
      deliveryManager ! OtherNodeSyncingStatus(cp1, Older, None)

      val header: ModifierId = headersIds.head

      deliveryManager ! RequestFromLocal(cp1, Header.modifierTypeId, Seq(header))

      //await one re-ask
      handler1.expectMsgAllOf(
        settings.network.deliveryTimeout * (settings.network.maxDeliveryChecks + 2),
        RequestModifiersNetworkMessage(Header.modifierTypeId -> Seq(header)),
        RequestModifiersNetworkMessage(Header.modifierTypeId -> Seq(header))
      )

      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(Header.modifierTypeId,
        Map(header -> Array.emptyByteArray)), cp1)

      handler1.expectMsg(SyncInfoNetworkMessage(EncrySyncInfo(List())))
    }
    "not re-ask modifiers which were applied to the history" in {
      val (deliveryManager, _, _, _, blocks, headerIds, _, history) = initialiseState()

      val address1 = new InetSocketAddress("123.123.123.123", 9001)
      val handler1: TestProbe = TestProbe()
      val cp1: ConnectedPeer = ConnectedPeer(address1, handler1.ref, Incoming,
        Handshake(protocolToBytes(settings.network.appVersion),
          "123.123.123.123", Some(address1), System.currentTimeMillis()))

      deliveryManager ! HandshakedPeer(cp1)
      deliveryManager ! OtherNodeSyncingStatus(cp1, Older, None)

      deliveryManager ! RequestFromLocal(cp1, Header.modifierTypeId, Seq(headerIds.head))

      handler1.expectMsg(RequestModifiersNetworkMessage(Header.modifierTypeId -> Seq(headerIds.head)))

      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(Header.modifierTypeId,
        Map(headerIds.head -> Array.emptyByteArray)), cp1)

      val uHistory: EncryHistory = history.append(blocks.head.header).get._1.reportModifierIsValid(blocks.head.header)

      deliveryManager ! UpdatedHistory(uHistory)

      deliveryManager ! SemanticallySuccessfulModifier(blocks.head.header)

      deliveryManager ! RequestFromLocal(cp1, Header.modifierTypeId, Seq(headerIds.head))

      handler1.expectMsg(SyncInfoNetworkMessage(EncrySyncInfo(List())))

      assert(deliveryManager.underlyingActor.expectedModifiers
        .getOrElse(cp1.socketAddress.getAddress, Map.empty).isEmpty)
    }
    "remove peer from expectedModifiers if expected modifiers collection from this peer is empty" in {
      val (deliveryManager, cp1, _, _, _, headerIds, _, _) = initialiseState()

      deliveryManager ! RequestFromLocal(cp1, Header.modifierTypeId, Seq(headerIds.head))
      //this thread sleep is using for expecting modifier removal
      Thread.sleep((settings.network.maxDeliveryChecks * settings.network.deliveryTimeout._1) * 1000)
      assert(deliveryManager.underlyingActor.expectedModifiers.getOrElse(cp1.socketAddress.getAddress, Map.empty).isEmpty)
      assert(deliveryManager.underlyingActor.expectedModifiers
        .getOrElse(cp1.socketAddress.getAddress, Map.empty) == Map.empty)
    }
    "not re-ask transactions" in {
      val (deliveryManager, _, _, _, _, _, _, _) = initialiseState()

      val address1 = new InetSocketAddress("123.123.123.123", 9001)
      val handler1: TestProbe = TestProbe()
      val cp1: ConnectedPeer = ConnectedPeer(address1, handler1.ref, Incoming,
        Handshake(protocolToBytes(settings.network.appVersion),
          "123.123.123.123", Some(address1), System.currentTimeMillis()))

      deliveryManager ! HandshakedPeer(cp1)
      deliveryManager ! OtherNodeSyncingStatus(cp1, Older, None)

      val transactions: Seq[ModifierId] = genValidPaymentTxs(1).map(_.id)

      deliveryManager ! RequestFromLocal(cp1, Transaction.ModifierTypeId, transactions)

      handler1.expectMsgAllOf(
        RequestModifiersNetworkMessage(Transaction.ModifierTypeId -> transactions)
      )
      handler1.expectNoMsg(10.seconds)
      assert(deliveryManager.underlyingActor.expectedModifiers
        .getOrElse(cp1.socketAddress.getAddress, Map.empty) == Map.empty)
    }
  }
}