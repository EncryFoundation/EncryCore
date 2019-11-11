package encry.network.DeliveryManagerTests

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestKit, TestProbe}
import encry.consensus.HistoryConsensus.{Fork, Unknown}
import encry.modifiers.InstanceFactory
import encry.network.DeliveryManager.FullBlockChainIsSynced
import encry.network.DeliveryManagerTests.DMUtils.{createPeer, generateBlocks, generateDummyHistory}
import encry.network.NetworkController.ReceivableMessages.DataFromPeer
import encry.network.{DeliveryManager, NodeViewSynchronizer}
import encry.network.NodeViewSynchronizer.ReceivableMessages.{ChangedHistory, RequestFromLocal, UpdatedHistory}
import encry.network.PeerConnectionHandler.{ConnectedPeer, Incoming}
import encry.network.PeersKeeper.UpdatedPeersCollection
import encry.network.PrioritiesCalculator.PeersPriorityStatus.PeersPriorityStatus.InitialPriority
import encry.settings.TestNetSettings
import encry.view.history.History
import encry.view.mempool.MemoryPool.StopTransactionsValidation
import org.encryfoundation.common.modifiers.history.{Block, Header}
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.network.BasicMessagesRepo.{Handshake, InvNetworkMessage, ModifiersNetworkMessage, RequestModifiersNetworkMessage, SyncInfoNetworkMessage}
import org.encryfoundation.common.network.SyncInfo
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike}

class DeliveryManagerSpec extends TestKit(ActorSystem("RequestModifiersSpec"))
  with WordSpecLike
  with BeforeAndAfterAll
  with MockitoSugar
  with Matchers
  with InstanceFactory
  with OneInstancePerTest
  with TestNetSettings {

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "DeliveryManager" should {
    "correctly request modifiers from forked node" in {

      val nvs: TestActorRef[NodeViewSynchronizer] =
        TestActorRef[NodeViewSynchronizer](NodeViewSynchronizer.props(None, TestProbe().ref, settings, TestProbe().ref, TestProbe().ref))

      val address = new InetSocketAddress("123.123.123.124", 9001)
      val handler: TestProbe = TestProbe()
      val cp: ConnectedPeer = ConnectedPeer(address, handler.ref, Incoming,
        Handshake(protocolToBytes(testNetSettings.network.appVersion),
          "123.123.123.124", Some(address), System.currentTimeMillis()))

      val initHistory = generateDummyHistory(testNetSettings)
      val otherHistory = generateDummyHistory(testNetSettings)

      val (_, blocks) = generateBlocks(100, generateDummyHistory(testNetSettings))

      val ourHistory = blocks.foldLeft(initHistory) { case (hst, block) =>
        hst.append(block.header)
        hst.append(block.payload)
        hst.reportModifierIsValid(block)
      }

      val commonHistory = blocks.take(60).foldLeft(otherHistory) { case (hst, block) =>
        hst.append(block.header)
        hst.append(block.payload)
        hst.reportModifierIsValid(block)
      }

      val (forkedHistory, forkedBlocks) = generateBlocks(40, commonHistory)

      val syncInfo = SyncInfo((blocks.slice(50, 60) ++ forkedBlocks).map(_.id))

      nvs ! ChangedHistory(ourHistory)

      nvs ! FullBlockChainIsSynced

      nvs ! UpdatedPeersCollection(Map(address -> (cp, Fork, InitialPriority)))

      nvs ! DataFromPeer(SyncInfoNetworkMessage(syncInfo), cp)

      handler.expectMsg(InvNetworkMessage(Header.modifierTypeId -> blocks.drop(60).map(_.id)))

      val idsForLocalRequest = forkedBlocks.map(_.id).filterNot(ourHistory.isModifierDefined)

      nvs ! RequestFromLocal(cp, Header.modifierTypeId, idsForLocalRequest)

      handler.expectMsg(RequestModifiersNetworkMessage(Header.modifierTypeId -> idsForLocalRequest))
      nvs.stop()
    }

    "not process transactions if number of transactions in mempool exceeds limit" in {

      val downloadedModifiersValidator = TestProbe()
      val address = new InetSocketAddress("123.123.123.124", 9001)
      val handler: TestProbe = TestProbe()
      val cp: ConnectedPeer = ConnectedPeer(address, handler.ref, Incoming,
        Handshake(protocolToBytes(testNetSettings.network.appVersion),
          "123.123.123.124", Some(address), System.currentTimeMillis()))

      val deliveryManager: TestActorRef[DeliveryManager] =
        TestActorRef[DeliveryManager](DeliveryManager
          .props(None, TestProbe().ref, TestProbe().ref, TestProbe().ref, TestProbe().ref, downloadedModifiersValidator.ref, settings))

      val history: History = generateDummyHistory(settings)

      deliveryManager ! UpdatedHistory(history)
      deliveryManager ! StopTransactionsValidation

      val txs: Map[ModifierId, Array[Byte]] = genValidPaymentTxs(100).groupBy(_.id).mapValues(_.head.bytes)
      val msg = ModifiersNetworkMessage(Transaction.modifierTypeId, txs)

      deliveryManager ! DataFromPeer(msg, cp)
      downloadedModifiersValidator.expectNoMsg()
      deliveryManager.stop()
    }
  }

}
