package encry.network.DeliveryManagerTests

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestProbe}
import encry.modifiers.InstanceFactory
import encry.network.BasicMessagesRepo.Handshake
import encry.network.DeliveryManager
import encry.network.NodeViewSynchronizer.ReceivableMessages.{CheckModifiersToDownload, DisconnectedPeer, HandshakedPeer, UpdatedHistory}
import encry.network.PeerConnectionHandler.{ConnectedPeer, Incoming}
import encry.settings.EncryAppSettings
import encry.view.history.EncryHistory
import org.scalatest.{BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike}

class DeliveryManagerInitialBehaviourSpec extends WordSpecLike with BeforeAndAfterAll
  with Matchers
  with InstanceFactory
  with OneInstancePerTest {

  implicit val system: ActorSystem = ActorSystem("SynchronousTestingSpec")
  val settings: EncryAppSettings = EncryAppSettings.read

  override def afterAll(): Unit = system.terminate()

  "DeliveryManager" should {
    "handle only HistoryChange, HandshakedPeer, DisconnectedPeer " +
      "and don't handle other messages before history's initialisation" in {
      val history: EncryHistory = generateDummyHistory(settings)
      val deliveryManager: TestActorRef[DeliveryManager] =
        TestActorRef[DeliveryManager](DeliveryManager.props(None, TestProbe().ref, TestProbe().ref, settings, TestProbe().ref))
      val newPeer = new InetSocketAddress("172.16.13.10", 9001)
      val newPeer2 = new InetSocketAddress("172.16.13.11", 9001)
      val peer: ConnectedPeer = ConnectedPeer(newPeer, deliveryManager, Incoming,
        Handshake(protocolToBytes(settings.network.appVersion), "peer", Some(newPeer), System.currentTimeMillis()))
      val peer2: ConnectedPeer = ConnectedPeer(newPeer2, deliveryManager, Incoming,
        Handshake(protocolToBytes(settings.network.appVersion), "peer", Some(newPeer2), System.currentTimeMillis()))

      deliveryManager ! HandshakedPeer(peer)
      assert(deliveryManager.underlyingActor.syncTracker.statuses.contains(peer.socketAddress.getAddress))
      assert(deliveryManager.underlyingActor.syncTracker.statuses.size == 1)
      deliveryManager ! HandshakedPeer(peer2)
      assert(deliveryManager.underlyingActor.syncTracker.statuses.contains(peer2.socketAddress.getAddress))
      assert(deliveryManager.underlyingActor.syncTracker.statuses.size == 2)
      deliveryManager ! DisconnectedPeer(peer.socketAddress)
      assert(deliveryManager.underlyingActor.syncTracker.statuses.contains(peer2.socketAddress.getAddress))
      assert(deliveryManager.underlyingActor.syncTracker.statuses.size == 1)
      deliveryManager ! CheckModifiersToDownload
      deliveryManager ! UpdatedHistory(history)
      deliveryManager.stop()
    }
  }
}