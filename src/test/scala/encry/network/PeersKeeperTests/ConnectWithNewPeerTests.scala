package encry.network.PeersKeeperTests

import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestProbe}
import encry.modifiers.InstanceFactory
import encry.network.PeerConnectionHandler.{ConnectedPeer, Outgoing}
import encry.network.PeersKeeper._
import encry.network.{NetworkUtils, PeersKeeper}
import encry.settings.EncryAppSettings
import org.encryfoundation.common.network.BasicMessagesRepo.Handshake
import org.scalatest.{BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike}

class ConnectWithNewPeerTests extends WordSpecLike
  with Matchers
  with BeforeAndAfterAll
  with InstanceFactory
  with OneInstancePerTest {

  implicit val system: ActorSystem = ActorSystem()
  val settingsWithKnownPeers: EncryAppSettings = NetworkUtils.TestNetworkSettings.read("AdditionalTestSettings.conf")
  val settingsWithAllPeers: EncryAppSettings = NetworkUtils.TestNetworkSettings.read("MainTestSetting.conf")

  "Peers keeper" should {
    "correctly handle successful connection process" in {
      val networkController = TestProbe()
      val peersSenderProbe = TestProbe()
      val peersKeeper: TestActorRef[PeersKeeper] = TestActorRef[PeersKeeper](PeersKeeper.props(settingsWithKnownPeers, TestProbe().ref))
      val connectedPeer: ConnectedPeer = ConnectedPeer(settingsWithAllPeers.network.knownPeers.head, peersSenderProbe.ref, Outgoing,
        Handshake(protocolToBytes(settingsWithAllPeers.network.appVersion),
          "test-peer", Some(settingsWithAllPeers.network.knownPeers.head), System.currentTimeMillis()))

      networkController.send(peersKeeper, RequestPeerForConnection)
      networkController.expectMsg(PeerForConnection(settingsWithAllPeers.network.knownPeers.head))
      peersKeeper.underlyingActor.outgoingConnections.contains(settingsWithAllPeers.network.knownPeers.head) shouldBe true
      peersKeeper.underlyingActor.availablePeers.contains(settingsWithAllPeers.network.knownPeers.head) shouldBe true

      networkController.send(peersKeeper, RequestForStableConnection(settingsWithAllPeers.network.knownPeers.head, peersSenderProbe.ref))
      networkController.expectMsg(
        CreateStableConnection(settingsWithAllPeers.network.knownPeers.head, peersSenderProbe.ref, Outgoing))

      networkController.send(peersKeeper, StableConnectionSetup(connectedPeer))
      peersKeeper.underlyingActor.connectedPeers.contains(settingsWithAllPeers.network.knownPeers.head) shouldBe true
    }
    "correctly handle stop connection process" in {
      val networkController = TestProbe()
      val peersSenderProbe = TestProbe()
      val peersKeeper: TestActorRef[PeersKeeper] = TestActorRef[PeersKeeper](PeersKeeper.props(settingsWithKnownPeers, TestProbe().ref))
      val connectedPeer: ConnectedPeer = ConnectedPeer(settingsWithAllPeers.network.knownPeers.head, peersSenderProbe.ref, Outgoing,
        Handshake(protocolToBytes(settingsWithAllPeers.network.appVersion),
          "test-peer", Some(settingsWithAllPeers.network.knownPeers.head), System.currentTimeMillis()))

      networkController.send(peersKeeper, RequestPeerForConnection)
      networkController.expectMsg(PeerForConnection(settingsWithAllPeers.network.knownPeers.head))
      peersKeeper.underlyingActor.outgoingConnections.contains(settingsWithAllPeers.network.knownPeers.head) shouldBe true
      peersKeeper.underlyingActor.availablePeers.contains(settingsWithAllPeers.network.knownPeers.head) shouldBe true

      networkController.send(peersKeeper, RequestForStableConnection(settingsWithAllPeers.network.knownPeers.head, peersSenderProbe.ref))
      networkController.expectMsg(
        CreateStableConnection(settingsWithAllPeers.network.knownPeers.head, peersSenderProbe.ref, Outgoing))

      networkController.send(peersKeeper, StableConnectionSetup(connectedPeer))
      peersKeeper.underlyingActor.connectedPeers.contains(settingsWithAllPeers.network.knownPeers.head) shouldBe true

      peersKeeper ! ConnectionStopped(settingsWithAllPeers.network.knownPeers.head)
      peersKeeper.underlyingActor.connectedPeers.contains(settingsWithAllPeers.network.knownPeers.head) shouldBe false
      peersKeeper.underlyingActor.availablePeers.contains(settingsWithAllPeers.network.knownPeers.head) shouldBe true
    }
    "correctly handle failed connection process" in {
      val networkController = TestProbe()
      val peersSenderProbe = TestProbe()
      val peersKeeper: TestActorRef[PeersKeeper] = TestActorRef[PeersKeeper](PeersKeeper.props(settingsWithKnownPeers, TestProbe().ref))

      networkController.send(peersKeeper, RequestPeerForConnection)
      networkController.expectMsg(PeerForConnection(settingsWithAllPeers.network.knownPeers.head))
      peersKeeper.underlyingActor.outgoingConnections.contains(settingsWithAllPeers.network.knownPeers.head) shouldBe true
      peersKeeper.underlyingActor.availablePeers.contains(settingsWithAllPeers.network.knownPeers.head) shouldBe true

      networkController.send(peersKeeper, RequestForStableConnection(settingsWithAllPeers.network.knownPeers.head, peersSenderProbe.ref))
      networkController.expectMsg(
        CreateStableConnection(settingsWithAllPeers.network.knownPeers.head, peersSenderProbe.ref, Outgoing))

      peersKeeper ! OutgoingConnectionFailed(settingsWithAllPeers.network.knownPeers.head)
      peersKeeper.underlyingActor.outgoingConnections.contains(settingsWithAllPeers.network.knownPeers.head) shouldBe false
      peersKeeper.underlyingActor.availablePeers.contains(settingsWithAllPeers.network.knownPeers.head) shouldBe true
    }
  }
}