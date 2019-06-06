package encry.network.PeersKeeperTests

import java.net.InetSocketAddress
import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestProbe}
import encry.modifiers.InstanceFactory
import encry.network.BlackList.SyntacticallyInvalidModifier
import encry.network.PeerConnectionHandler.{ConnectedPeer, Incoming, Outgoing}
import encry.network.PeersKeeper.{BanPeer, CreateStableConnection, RequestForStableConnection, RequestPeerForConnection, StableConnectionSetup}
import encry.network.{NetworkUtils, PeersKeeper}
import encry.settings.EncryAppSettings
import org.encryfoundation.common.network.BasicMessagesRepo.Handshake
import org.scalatest.{BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike}

class IncomingOutgoingConnectionsTests extends WordSpecLike
  with Matchers
  with BeforeAndAfterAll
  with InstanceFactory
  with OneInstancePerTest {

  implicit val system: ActorSystem = ActorSystem()
  val settingsWithKnownPeers: EncryAppSettings = NetworkUtils.TestNetworkSettings.read("AdditionalTestSettings.conf")
  val settingsWithAllPeers: EncryAppSettings = NetworkUtils.TestNetworkSettings.read("MainTestSetting.conf")

  "Peer keeper" should {
    "handle incoming connections correctly while connection with only known peers false " +
      "and incoming peer doesn't contain in black list and connected peers" in {
      val networkController = TestProbe()
      val remoteConnectionTestProbe: TestProbe = TestProbe()
      val remoteAddress: InetSocketAddress = new InetSocketAddress("172.16.11.11", 9001)
      val peersKeeper: TestActorRef[PeersKeeper] = TestActorRef[PeersKeeper](PeersKeeper.props(settingsWithAllPeers, TestProbe().ref))

      networkController.send(peersKeeper, RequestForStableConnection(remoteAddress, remoteConnectionTestProbe.ref))
      networkController.expectMsg(CreateStableConnection(remoteAddress, remoteConnectionTestProbe.ref, Incoming))
      peersKeeper.stop()
    }
    "handle incoming connections correctly while connection with only known peers false " +
      "and incoming peer contain in black list" in {
      val networkController = TestProbe()
      val remoteConnectionTestProbe: TestProbe = TestProbe()
      val remoteAddress: InetSocketAddress = new InetSocketAddress("172.16.11.11", 9001)
      val peersKeeper: TestActorRef[PeersKeeper] = TestActorRef[PeersKeeper](PeersKeeper.props(settingsWithAllPeers, TestProbe().ref))
      val connectedPeer: ConnectedPeer = ConnectedPeer(remoteAddress, remoteConnectionTestProbe.ref, Incoming,
        Handshake(protocolToBytes(settingsWithAllPeers.network.appVersion),
          "test-peer", Some(remoteAddress), System.currentTimeMillis()))

      peersKeeper ! BanPeer(connectedPeer, SyntacticallyInvalidModifier)
      networkController.send(peersKeeper, RequestForStableConnection(remoteAddress, remoteConnectionTestProbe.ref))
      networkController.expectNoMsg()
      peersKeeper.stop()
    }
    "handle incoming connections correctly while connection with only known peers false " +
      "and incoming peer contain in connected peers" in {
      val networkController = TestProbe()
      val remoteConnectionTestProbe: TestProbe = TestProbe()
      val remoteAddress: InetSocketAddress = new InetSocketAddress("172.16.11.11", 9001)
      val peersKeeper: TestActorRef[PeersKeeper] = TestActorRef[PeersKeeper](PeersKeeper.props(settingsWithAllPeers, TestProbe().ref))
      val connectedPeer: ConnectedPeer = ConnectedPeer(remoteAddress, remoteConnectionTestProbe.ref, Incoming,
        Handshake(protocolToBytes(settingsWithAllPeers.network.appVersion),
          "test-peer", Some(remoteAddress), System.currentTimeMillis()))

      peersKeeper ! StableConnectionSetup(connectedPeer)
      networkController.send(peersKeeper, RequestForStableConnection(remoteAddress, remoteConnectionTestProbe.ref))
      networkController.expectNoMsg()
      peersKeeper.stop()
    }
    "handle incoming connections correctly while connection with only known peers true" in {
      val networkController = TestProbe()
      val remoteConnectionTestProbe: TestProbe = TestProbe()
      val remoteAddress: InetSocketAddress = new InetSocketAddress("172.16.11.11", 9001)
      val peersKeeper: TestActorRef[PeersKeeper] = TestActorRef[PeersKeeper](PeersKeeper.props(settingsWithKnownPeers, TestProbe().ref))

      networkController.send(peersKeeper, RequestForStableConnection(remoteAddress, remoteConnectionTestProbe.ref))
      networkController.expectNoMsg()
      peersKeeper.stop()
    }
    "handle incoming connections correctly while peer is equal to local address" in {
      val networkController = TestProbe()
      val remoteConnectionTestProbe: TestProbe = TestProbe()
      val peersKeeper: TestActorRef[PeersKeeper] = TestActorRef[PeersKeeper](PeersKeeper.props(settingsWithAllPeers, TestProbe().ref))

      networkController.send(peersKeeper, RequestForStableConnection(
        new InetSocketAddress("0.0.0.0", 9001), remoteConnectionTestProbe.ref))
      networkController.expectNoMsg()
      peersKeeper.stop()
    }

    "Peers keeper" should {
      "correctly handle outgoing connection" in {
        val networkController = TestProbe()
        val remoteConnectionTestProbe: TestProbe = TestProbe()
        val peersKeeper: TestActorRef[PeersKeeper] = TestActorRef[PeersKeeper](PeersKeeper.props(settingsWithKnownPeers, TestProbe().ref))

        peersKeeper ! RequestPeerForConnection
        peersKeeper.underlyingActor.outgoingConnections.contains(settingsWithKnownPeers.network.knownPeers.head) shouldBe true
        networkController.send(peersKeeper, RequestForStableConnection(settingsWithKnownPeers.network.knownPeers.head, remoteConnectionTestProbe.ref))
        networkController.expectMsg(
          CreateStableConnection(settingsWithKnownPeers.network.knownPeers.head, remoteConnectionTestProbe.ref, Outgoing))
      }
    }
  }
}