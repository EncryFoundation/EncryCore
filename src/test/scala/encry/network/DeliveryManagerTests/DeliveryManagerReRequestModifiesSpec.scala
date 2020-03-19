package encry.network.DeliveryManagerTests

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestProbe}
import encry.consensus.HistoryConsensus
import encry.consensus.HistoryConsensus.Older
import encry.modifiers.InstanceFactory
import encry.network.DM.RequestSent
import encry.network.{DM, DeliveryManager}
import encry.network.DeliveryManagerTests.DMUtils._
import encry.network.Messages.MessageToNetwork.RequestFromLocal
import encry.network.NetworkController.ReceivableMessages.{DataFromPeer, RegisterMessagesHandler}
import encry.network.NodeViewSynchronizer.ReceivableMessages._
import encry.network.PeerConnectionHandler.{ConnectedPeer, Incoming}
import encry.network.PeersKeeper.UpdatedPeersCollection
import encry.network.PrioritiesCalculator.PeersPriorityStatus.PeersPriorityStatus.InitialPriority
import encry.network.PrioritiesCalculator.PeersPriorityStatus.PeersPriorityStatus
import encry.nvg.NodeViewHolder.SemanticallySuccessfulModifier
import encry.settings.TestNetSettings
import encry.view.history.History
import org.encryfoundation.common.modifiers.history.{Block, Header, HeaderProtoSerializer}
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.network.BasicMessagesRepo.{GetPeersNetworkMessage, Handshake, ModifiersNetworkMessage, PeersNetworkMessage, RequestModifiersNetworkMessage}
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
import org.scalatest.{BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike}

import scala.concurrent.duration._
import scala.collection.mutable.WrappedArray

class DeliveryManagerReRequestModifiesSpec extends WordSpecLike
  with BeforeAndAfterAll
  with Matchers
  with InstanceFactory
  with OneInstancePerTest
  with TestNetSettings {

  implicit val system: ActorSystem = ActorSystem("SynchronousTestingSpec")

  override def afterAll(): Unit = system.terminate()

  def initialiseState(isChainSynced: Boolean = true, isMining: Boolean = true): (TestProbe, TestActorRef[DM],
    ConnectedPeer, ConnectedPeer, ConnectedPeer, List[Block], List[ModifierId], List[WrappedArray.ofByte], History) = {
    val (networkRouter, deliveryManager, history) = initialiseDeliveryManager(isBlockChainSynced = isChainSynced, isMining = isMining, testNetSettings)
    val (_: InetSocketAddress, cp1: ConnectedPeer) = createPeer(9001, "172.16.13.10", testNetSettings)
    val (_: InetSocketAddress, cp2: ConnectedPeer) = createPeer(9002, "172.16.13.11", testNetSettings)
    val (_: InetSocketAddress, cp3: ConnectedPeer) = createPeer(9003, "172.16.13.12", testNetSettings)
    val blocks: List[Block] = generateBlocks(10, generateDummyHistory(testNetSettings))._2
    val headersIds: List[ModifierId] = blocks.map(_.header.id)
    val headersAsKey = headersIds.map(toKey)
    (networkRouter, deliveryManager, cp1, cp2, cp3, blocks, headersIds, headersAsKey, history)
  }

  "ReRequestModifies" should {
    "re-ask necessary modifier several times (number of attempts from testNetSettings) and remove modifier from " +
        //todo: move to message builder tests
//      "expectedModifiers collection after all attempts will expire" in {
//      val (deliveryManager, _, _, _, _, headersIds, _, _) = initialiseState()
//
//      val address1 = new InetSocketAddress("123.123.123.123", 9001)
//      val handler1: TestProbe = TestProbe()
//      val cp1: ConnectedPeer = ConnectedPeer(address1, handler1.ref, Incoming,
//        Handshake(protocolToBytes(testNetSettings.network.appVersion),
//          "123.123.123.123", Some(address1), System.currentTimeMillis()))
//
//      val updatedPeersCollection: Map[InetSocketAddress, (ConnectedPeer, HistoryConsensus.Older.type, PeersPriorityStatus)] =
//        Map(address1 -> (cp1, Older, InitialPriority))
//
//      deliveryManager ! UpdatedPeersCollection(updatedPeersCollection)
//
//      val header: ModifierId = headersIds.head
//
//      deliveryManager ! RequestSent(cp1.socketAddress, Header.modifierTypeId, header)
//      handler1.expectMsgAllOf(
//        testNetSettings.network.deliveryTimeout * (testNetSettings.network.maxDeliveryChecks + 2),
//        RequestModifiersNetworkMessage(Header.modifierTypeId -> Seq(header)),
//        RequestModifiersNetworkMessage(Header.modifierTypeId -> Seq(header)),
//        RequestModifiersNetworkMessage(Header.modifierTypeId -> Seq(header))
//      )
//      //this thread sleep is using for expecting modifier removal
//      Thread.sleep(6000)
//
//      assert(deliveryManager.underlyingActor.expectedModifiers.isEmpty)
//      deliveryManager.stop()
//    }
    "not re-ask unnecessary modifiers" in {
      val (networkRouter, deliveryManager, _, _, _, _, headersIds, _, _) = initialiseState()
      networkRouter.expectMsg(RegisterMessagesHandler(Seq(
        ModifiersNetworkMessage.NetworkMessageTypeID -> "ModifiersNetworkMessage",
      ), deliveryManager.underlying.self))
      val address1 = new InetSocketAddress("123.123.123.123", 9001)
      val handler1: TestProbe = TestProbe()
      val cp1: ConnectedPeer = ConnectedPeer(address1, handler1.ref, Incoming,
        Handshake(protocolToBytes(testNetSettings.network.appVersion),
          "123.123.123.123", Some(address1), System.currentTimeMillis()))

      val header: ModifierId = headersIds.head

      deliveryManager ! RequestSent(cp1.socketAddress, Header.modifierTypeId, header)

      //await one re-ask
      networkRouter.expectMsgAllOf(
        testNetSettings.network.deliveryTimeout * (testNetSettings.network.maxDeliveryChecks + 2),
        RequestFromLocal(Some(address1), Header.modifierTypeId, List(header)),
        RequestFromLocal(Some(address1), Header.modifierTypeId, List(header))
      )

      val headerBytes: Array[Byte] = HeaderProtoSerializer.toProto(genHeader).toByteArray

      deliveryManager ! DataFromPeer(ModifiersNetworkMessage(Header.modifierTypeId,
        Map(header -> headerBytes)), cp1.socketAddress)
      deliveryManager.stop()
    }
    "remove peer from expectedModifiers if expected modifiers collection from this peer is empty" in {
      val (_, deliveryManager, cp1, _, _, _, headerIds, _, _) = initialiseState()

      deliveryManager ! RequestSent(cp1.socketAddress, Header.modifierTypeId, headerIds.head)
      //this thread sleep is using for expecting modifier removal
      Thread.sleep((testNetSettings.network.maxDeliveryChecks * testNetSettings.network.deliveryTimeout._1) * 2000)
      assert(deliveryManager.underlyingActor.expectedModifiers.isEmpty)
      deliveryManager.stop()
    }
    "not re-ask transactions" in {
      val (networkRouter, deliveryManager, _, _, _, _, _, _, _) = initialiseState()
      networkRouter.expectMsg(RegisterMessagesHandler(Seq(
        ModifiersNetworkMessage.NetworkMessageTypeID -> "ModifiersNetworkMessage",
      ), deliveryManager.underlying.self))
      val address1 = new InetSocketAddress("123.123.123.123", 9001)

      val transactions: Seq[ModifierId] = genValidPaymentTxs(1).map(_.id)

      transactions.foreach(txId => deliveryManager ! RequestSent(address1, Transaction.modifierTypeId, txId))

      networkRouter.expectNoMsg(testNetSettings.network.deliveryTimeout + 10.seconds)
      assert(deliveryManager.underlyingActor.expectedModifiers.isEmpty)
      deliveryManager.stop()
    }
  }
}