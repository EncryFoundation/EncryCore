package encry.network

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestProbe}
import encry.modifiers.InstanceFactory
import encry.network.BlackList.BanReason._
import encry.network.NetworkController.ReceivableMessages.DataFromPeer
import encry.network.PeerConnectionHandler.{ConnectedPeer, Incoming, Outgoing}
import encry.network.PeersKeeper._
import encry.settings.TestNetSettings
import org.encryfoundation.common.network.BasicMessagesRepo.{Handshake, PeersNetworkMessage}
import org.scalatest.{BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike}
import scala.concurrent.duration._

class ConnectWithNewPeerTests extends WordSpecLike
  with Matchers
  with BeforeAndAfterAll
  with InstanceFactory
  with OneInstancePerTest
  with TestNetSettings {

  implicit val system: ActorSystem = ActorSystem()

  override def afterAll(): Unit = system.terminate()

  val knowPeersSettings = testNetSettings.copy(
    network = testNetSettings.network.copy(
      knownPeers = List(new InetSocketAddress("172.16.11.11", 9001)),
      connectOnlyWithKnownPeers = Some(true)
    ),
    blackList = testNetSettings.blackList.copy(
      banTime = 2 seconds,
      cleanupTime = 3 seconds
    ))

  "Peers keeper" should {
    //    "maintain outgoing connection process correctly" in {
    //      /* Request first peer while current number of connections is 0 */
    //      val networkController = TestProbe()
    //      val nodeViewSync = TestProbe()
    //      val peersKeeper: TestActorRef[PeersKeeper] = TestActorRef[PeersKeeper](PeersKeeper.props(testNetSettingsWithAllPeers, nodeViewSync.ref, TestProbe().ref))
    //
    //      val availablePeers: Map[InetSocketAddress, Int] = peersKeeper.underlyingActor.knownPeers
    //
    //      networkController.send(peersKeeper, RequestPeerForConnection)
    //      networkController.expectMsg(PeerForConnection(availablePeers.head._1))
    //      peersKeeper.underlyingActor.outgoingConnections.contains(availablePeers.head._1) shouldBe true
    //      peersKeeper.underlyingActor.awaitingHandshakeConnections.contains(availablePeers.head._1) shouldBe true
    //
    //      val remoteAkkaConnectionHandler = TestProbe()
    //
    //      networkController.send(peersKeeper, VerifyConnection(availablePeers.head._1, remoteAkkaConnectionHandler.ref))
    //      networkController.expectMsg(ConnectionVerified(availablePeers.head._1, remoteAkkaConnectionHandler.ref, Outgoing))
    //      peersKeeper.underlyingActor.outgoingConnections.contains(availablePeers.head._1) shouldBe false
    //      peersKeeper.underlyingActor.awaitingHandshakeConnections.contains(availablePeers.head._1) shouldBe true
    //
    //      val peerHandler = TestProbe()
    //      val connectedPeer: ConnectedPeer = ConnectedPeer(availablePeers.head._1, peerHandler.ref, Outgoing,
    //        Handshake(protocolToBytes(testNetSettingsWithAllPeers.network.appVersion),
    //          "test-peer", Some(availablePeers.head._1), System.currentTimeMillis()))
    //
    //      networkController.send(peersKeeper, HandshakedDone(connectedPeer))
    //      peersKeeper.underlyingActor.awaitingHandshakeConnections.contains(availablePeers.head._1) shouldBe false
    //      peersKeeper.underlyingActor.knownPeers.contains(availablePeers.head._1) shouldBe true
    //      peersKeeper.underlyingActor.knownPeers.get(availablePeers.head._1) shouldBe Some(0)
    //
    //      /* Request next peer after first connection setup */
    //
    //      val newAvailablePeers: Map[InetSocketAddress, Int] = peersKeeper.underlyingActor.knownPeers.drop(1)
    //
    //      networkController.send(peersKeeper, RequestPeerForConnection)
    //      networkController.expectMsg(PeerForConnection(newAvailablePeers.head._1))
    //      peersKeeper.underlyingActor.outgoingConnections.contains(newAvailablePeers.head._1) shouldBe true
    //      peersKeeper.underlyingActor.awaitingHandshakeConnections.contains(newAvailablePeers.head._1) shouldBe true
    //
    //      networkController.send(peersKeeper, VerifyConnection(newAvailablePeers.head._1, remoteAkkaConnectionHandler.ref))
    //      networkController.expectMsg(ConnectionVerified(newAvailablePeers.head._1, remoteAkkaConnectionHandler.ref, Outgoing))
    //      peersKeeper.underlyingActor.outgoingConnections.contains(newAvailablePeers.head._1) shouldBe false
    //      peersKeeper.underlyingActor.awaitingHandshakeConnections.contains(newAvailablePeers.head._1) shouldBe true
    //
    //      val newPeerHandler = TestProbe()
    //      val newConnectedPeer: ConnectedPeer = ConnectedPeer(newAvailablePeers.head._1, newPeerHandler.ref, Outgoing,
    //        Handshake(protocolToBytes(testNetSettingsWithAllPeers.network.appVersion),
    //          "test-peer_new", Some(newAvailablePeers.head._1), System.currentTimeMillis()))
    //
    //      networkController.send(peersKeeper, HandshakedDone(newConnectedPeer))
    //      peersKeeper.underlyingActor.awaitingHandshakeConnections.contains(newAvailablePeers.head._1) shouldBe false
    //      peersKeeper.underlyingActor.knownPeers.contains(newAvailablePeers.head._1) shouldBe true
    //      peersKeeper.underlyingActor.knownPeers.get(newAvailablePeers.head._1) shouldBe Some(0)
    //
    //      /* Try to ask one more peer while max number of connections has been expired */
    //
    //      networkController.send(peersKeeper, RequestPeerForConnection)
    //      networkController.expectNoMsg()
    //
    //      /* Now we will ban one peer */
    //
    //      val actorWhichSendBanMessage = TestProbe()
    //
    //      actorWhichSendBanMessage.send(peersKeeper, BanPeer(newConnectedPeer, ExpiredNumberOfConnections))
    //      newPeerHandler.expectMsgAnyOf(CloseConnection, GetPeersNetworkMessage)
    //      peersKeeper.underlyingActor.blackList.contains(newConnectedPeer.socketAddress.getAddress) shouldBe true
    //      networkController.send(peersKeeper, ConnectionStopped(newConnectedPeer.socketAddress))
    //      peersKeeper.underlyingActor.knownPeers.contains(newConnectedPeer.socketAddress) shouldBe false
    //      peersKeeper.underlyingActor.connectedPeers.contains(newConnectedPeer.socketAddress) shouldBe false
    //
    //      /* Try to setup Incoming connection from banned peer */
    //
    //      networkController.send(peersKeeper, VerifyConnection(newConnectedPeer.socketAddress, remoteAkkaConnectionHandler.ref))
    //      networkController.expectNoMsg()
    //
    //      /* Try to request new connection */
    //
    //      val updatedAvailablePeers: Map[InetSocketAddress, Int] = peersKeeper.underlyingActor.knownPeers.takeRight(1)
    //
    //      networkController.send(peersKeeper, RequestPeerForConnection)
    //      networkController.expectMsg(PeerForConnection(updatedAvailablePeers.head._1))
    //      peersKeeper.underlyingActor.outgoingConnections.contains(updatedAvailablePeers.head._1) shouldBe true
    //      peersKeeper.underlyingActor.awaitingHandshakeConnections.contains(updatedAvailablePeers.head._1) shouldBe true
    //
    //      val updatedRemoteAkkaConnectionHandler = TestProbe()
    //
    //      networkController.send(peersKeeper, VerifyConnection(updatedAvailablePeers.head._1, updatedRemoteAkkaConnectionHandler.ref))
    //      networkController.expectMsg(ConnectionVerified(updatedAvailablePeers.head._1, updatedRemoteAkkaConnectionHandler.ref, Outgoing))
    //      peersKeeper.underlyingActor.outgoingConnections.contains(updatedAvailablePeers.head._1) shouldBe false
    //      peersKeeper.underlyingActor.awaitingHandshakeConnections.contains(updatedAvailablePeers.head._1) shouldBe true
    //
    //      val updatedConnectedPeer: ConnectedPeer = ConnectedPeer(updatedAvailablePeers.head._1, updatedRemoteAkkaConnectionHandler.ref, Outgoing,
    //        Handshake(protocolToBytes(testNetSettingsWithAllPeers.network.appVersion),
    //          "test-peer", Some(updatedAvailablePeers.head._1), System.currentTimeMillis()))
    //
    //      networkController.send(peersKeeper, HandshakedDone(updatedConnectedPeer))
    //      peersKeeper.underlyingActor.awaitingHandshakeConnections.contains(updatedAvailablePeers.head._1) shouldBe false
    //      peersKeeper.underlyingActor.knownPeers.contains(updatedAvailablePeers.head._1) shouldBe true
    //      peersKeeper.underlyingActor.knownPeers.get(updatedAvailablePeers.head._1) shouldBe Some(0)
    //    }
    //    "remove peer from available we can't connect to" in {
    //      val networkController = TestProbe()
    //      val nodeViewSync = TestProbe()
    //      val peersKeeper: TestActorRef[PeersKeeper] = TestActorRef[PeersKeeper](PeersKeeper.props(testNetSettingsWithAllPeers, nodeViewSync.ref, TestProbe().ref))
    //
    //      val availablePeers: Map[InetSocketAddress, Int] = peersKeeper.underlyingActor.knownPeers
    //
    //      networkController.send(peersKeeper, RequestPeerForConnection)
    //      networkController.expectMsg(PeerForConnection(availablePeers.head._1))
    //
    //      networkController.send(peersKeeper, OutgoingConnectionFailed(availablePeers.head._1))
    //      peersKeeper.underlyingActor.outgoingConnections.contains(availablePeers.head._1) shouldBe false
    //      peersKeeper.underlyingActor.awaitingHandshakeConnections.contains(availablePeers.head._1) shouldBe false
    //      peersKeeper.underlyingActor.knownPeers.get(availablePeers.head._1) shouldBe Some(1)
    //
    //      networkController.send(peersKeeper, RequestPeerForConnection)
    //      networkController.expectMsg(PeerForConnection(availablePeers.head._1))
    //
    //      networkController.send(peersKeeper, OutgoingConnectionFailed(availablePeers.head._1))
    //      peersKeeper.underlyingActor.outgoingConnections.contains(availablePeers.head._1) shouldBe false
    //      peersKeeper.underlyingActor.awaitingHandshakeConnections.contains(availablePeers.head._1) shouldBe false
    //      peersKeeper.underlyingActor.knownPeers.contains(availablePeers.head._1) shouldBe false
    //      peersKeeper.underlyingActor.blackList.contains(availablePeers.head._1.getAddress) shouldBe true
    //    }
    "remove peer from available if it has been banned" in {
      val networkController = TestProbe()
      val nodeViewSync = TestProbe()
      val peersKeeper: TestActorRef[PeersKeeper] = TestActorRef[PeersKeeper](PeersKeeper.props(testNetSettings, nodeViewSync.ref, TestProbe().ref))

      val availablePeers: Map[InetSocketAddress, Int] = peersKeeper.underlyingActor.knownPeers

      val peerHandler = TestProbe()
      val connectedPeer: ConnectedPeer = ConnectedPeer(availablePeers.head._1, peerHandler.ref, Outgoing,
        Handshake(protocolToBytes(testNetSettings.network.appVersion),
          "test-peer", Some(availablePeers.head._1), System.currentTimeMillis()))

      networkController.send(peersKeeper, BanPeer(connectedPeer, ExpiredNumberOfConnections))
      networkController.send(peersKeeper, ConnectionStopped(availablePeers.head._1))

      peersKeeper.underlyingActor.knownPeers.contains(availablePeers.head._1) shouldBe false
    }
    "filter peers from network message" in {
      val networkController = TestProbe()
      val nodeViewSync = TestProbe()
      val peersKeeper: TestActorRef[PeersKeeper] = TestActorRef[PeersKeeper](PeersKeeper.props(testNetSettings, nodeViewSync.ref, TestProbe().ref))

      val availablePeers: Map[InetSocketAddress, Int] = peersKeeper.underlyingActor.knownPeers

      val peerHandler = TestProbe()
      val connectedPeer: ConnectedPeer = ConnectedPeer(availablePeers.head._1, peerHandler.ref, Outgoing,
        Handshake(protocolToBytes(testNetSettings.network.appVersion),
          "test-peer", Some(availablePeers.head._1), System.currentTimeMillis()))

      val newPeerHandler = TestProbe()
      val newConnectedPeer: ConnectedPeer = ConnectedPeer(availablePeers.last._1, newPeerHandler.ref, Outgoing,
        Handshake(protocolToBytes(testNetSettings.network.appVersion),
          "test-peer", Some(availablePeers.last._1), System.currentTimeMillis()))

      networkController.send(peersKeeper, BanPeer(connectedPeer, ExpiredNumberOfConnections))
      networkController.send(peersKeeper, ConnectionStopped(availablePeers.head._1))
      networkController.send(peersKeeper, HandshakedDone(newConnectedPeer))
      peersKeeper.underlyingActor.knownPeers.contains(availablePeers.head._1) shouldBe false

      val peer = new InetSocketAddress("172.16.28.98", 9023)
      val peers = Seq(availablePeers.last._1, availablePeers.head._1, peer)

      networkController.send(peersKeeper, DataFromPeer(PeersNetworkMessage(peers), newConnectedPeer))
      peersKeeper.underlyingActor.knownPeers.contains(availablePeers.head._1) shouldBe false
      peersKeeper.underlyingActor.knownPeers.contains(peer) shouldBe true
    }
    "handle successful connection process" in {
      val networkController = TestProbe()
      val peersSenderProbe = TestProbe()
      val peersKeeper: TestActorRef[PeersKeeper] = TestActorRef[PeersKeeper](PeersKeeper.props(knowPeersSettings, TestProbe().ref, TestProbe().ref))
      val connectedPeer: ConnectedPeer = ConnectedPeer(testNetSettings.network.knownPeers.head, peersSenderProbe.ref, Outgoing,
        Handshake(protocolToBytes(testNetSettings.network.appVersion),
          "test-peer", Some(testNetSettings.network.knownPeers.head), System.currentTimeMillis()))

      networkController.send(peersKeeper, RequestPeerForConnection)
      networkController.expectMsg(PeerForConnection(testNetSettings.network.knownPeers.head))
      peersKeeper.underlyingActor.outgoingConnections.contains(testNetSettings.network.knownPeers.head) shouldBe true
      peersKeeper.underlyingActor.knownPeers.contains(testNetSettings.network.knownPeers.head) shouldBe true

      networkController.send(peersKeeper, VerifyConnection(testNetSettings.network.knownPeers.head, peersSenderProbe.ref))
      networkController.expectMsg(
        ConnectionVerified(testNetSettings.network.knownPeers.head, peersSenderProbe.ref, Outgoing))

      networkController.send(peersKeeper, HandshakedDone(connectedPeer))
      peersKeeper.underlyingActor.connectedPeers.contains(testNetSettings.network.knownPeers.head) shouldBe true
    }
    "handle stop connection process" in {
      val networkController = TestProbe()
      val peersSenderProbe = TestProbe()
      val peersKeeper: TestActorRef[PeersKeeper] = TestActorRef[PeersKeeper](PeersKeeper.props(knowPeersSettings, TestProbe().ref, TestProbe().ref))
      val connectedPeer: ConnectedPeer = ConnectedPeer(testNetSettings.network.knownPeers.head, peersSenderProbe.ref, Outgoing,
        Handshake(protocolToBytes(testNetSettings.network.appVersion),
          "test-peer", Some(testNetSettings.network.knownPeers.head), System.currentTimeMillis()))

      networkController.send(peersKeeper, RequestPeerForConnection)
      networkController.expectMsg(PeerForConnection(testNetSettings.network.knownPeers.head))
      peersKeeper.underlyingActor.outgoingConnections.contains(testNetSettings.network.knownPeers.head) shouldBe true
      peersKeeper.underlyingActor.knownPeers.contains(testNetSettings.network.knownPeers.head) shouldBe true

      networkController.send(peersKeeper, VerifyConnection(testNetSettings.network.knownPeers.head, peersSenderProbe.ref))
      networkController.expectMsg(
        ConnectionVerified(testNetSettings.network.knownPeers.head, peersSenderProbe.ref, Outgoing))

      networkController.send(peersKeeper, HandshakedDone(connectedPeer))
      peersKeeper.underlyingActor.connectedPeers.contains(testNetSettings.network.knownPeers.head) shouldBe true

      peersKeeper ! ConnectionStopped(testNetSettings.network.knownPeers.head)
      peersKeeper.underlyingActor.connectedPeers.contains(testNetSettings.network.knownPeers.head) shouldBe false
      peersKeeper.underlyingActor.knownPeers.contains(testNetSettings.network.knownPeers.head) shouldBe true
    }
    "handle failed connection process" in {
      val networkController = TestProbe()
      val peersSenderProbe = TestProbe()
      val peersKeeper: TestActorRef[PeersKeeper] = TestActorRef[PeersKeeper](PeersKeeper.props(knowPeersSettings, TestProbe().ref, TestProbe().ref))

      networkController.send(peersKeeper, RequestPeerForConnection)
      networkController.expectMsg(PeerForConnection(testNetSettings.network.knownPeers.head))
      peersKeeper.underlyingActor.outgoingConnections.contains(testNetSettings.network.knownPeers.head) shouldBe true
      peersKeeper.underlyingActor.knownPeers.contains(testNetSettings.network.knownPeers.head) shouldBe true

      networkController.send(peersKeeper, VerifyConnection(testNetSettings.network.knownPeers.head, peersSenderProbe.ref))
      networkController.expectMsg(
        ConnectionVerified(testNetSettings.network.knownPeers.head, peersSenderProbe.ref, Outgoing))

      peersKeeper ! OutgoingConnectionFailed(testNetSettings.network.knownPeers.head)
      peersKeeper.underlyingActor.outgoingConnections.contains(testNetSettings.network.knownPeers.head) shouldBe false
      peersKeeper.underlyingActor.knownPeers.contains(testNetSettings.network.knownPeers.head) shouldBe true
    }
    "handle incoming connections correctly while connection with only known peers false " +
      "and incoming peer doesn't contains in black list and connected peers collection" in {
      val networkController = TestProbe()
      val remoteConnectionTestProbe: TestProbe = TestProbe()
      val remoteAddress: InetSocketAddress = testNetSettings.network.knownPeers.head
      val peersKeeper: TestActorRef[PeersKeeper] = TestActorRef[PeersKeeper](PeersKeeper.props(testNetSettings, TestProbe().ref, TestProbe().ref))

      networkController.send(peersKeeper, VerifyConnection(remoteAddress, remoteConnectionTestProbe.ref))
      networkController.expectMsg(ConnectionVerified(remoteAddress, remoteConnectionTestProbe.ref, Incoming))
      peersKeeper.stop()
    }
    "handle incoming connections correctly while connection with only known peers false " +
      "and incoming peer contain in black list" in {
      val networkController = TestProbe()
      val remoteConnectionTestProbe: TestProbe = TestProbe()
      val remoteAddress: InetSocketAddress = new InetSocketAddress("172.16.11.11", 9001)
      val peersKeeper: TestActorRef[PeersKeeper] = TestActorRef[PeersKeeper](PeersKeeper.props(testNetSettings, TestProbe().ref, TestProbe().ref))
      val connectedPeer: ConnectedPeer = ConnectedPeer(remoteAddress, remoteConnectionTestProbe.ref, Incoming,
        Handshake(protocolToBytes(testNetSettings.network.appVersion),
          "test-peer", Some(remoteAddress), System.currentTimeMillis()))

      peersKeeper ! BanPeer(connectedPeer, SyntacticallyInvalidPersistentModifier)
      networkController.send(peersKeeper, VerifyConnection(remoteAddress, remoteConnectionTestProbe.ref))
      networkController.expectNoMsg()
      peersKeeper.stop()
    }
    "handle incoming connections correctly while connection with only known peers false " +
      "and incoming peer contain in connected peers" in {
      val networkController = TestProbe()
      val remoteConnectionTestProbe: TestProbe = TestProbe()
      val remoteAddress: InetSocketAddress = new InetSocketAddress("172.16.11.11", 9001)
      val peersKeeper: TestActorRef[PeersKeeper] = TestActorRef[PeersKeeper](PeersKeeper.props(testNetSettings, TestProbe().ref, TestProbe().ref))
      val connectedPeer: ConnectedPeer = ConnectedPeer(remoteAddress, remoteConnectionTestProbe.ref, Incoming,
        Handshake(protocolToBytes(testNetSettings.network.appVersion),
          "test-peer", Some(remoteAddress), System.currentTimeMillis()))

      peersKeeper ! HandshakedDone(connectedPeer)
      networkController.send(peersKeeper, VerifyConnection(remoteAddress, remoteConnectionTestProbe.ref))
      networkController.expectNoMsg()
      peersKeeper.stop()
    }
    "handle incoming connections correctly while connection with only known peers true" in {
      val networkController = TestProbe()
      val remoteConnectionTestProbe: TestProbe = TestProbe()
      val remoteAddress: InetSocketAddress = new InetSocketAddress("172.16.11.11", 9001)
      val peersKeeper: TestActorRef[PeersKeeper] = TestActorRef[PeersKeeper](PeersKeeper.props(knowPeersSettings, TestProbe().ref, TestProbe().ref))

      networkController.send(peersKeeper, VerifyConnection(remoteAddress, remoteConnectionTestProbe.ref))
      networkController.expectNoMsg()
      peersKeeper.stop()
    }
    "handle incoming connections correctly while peer is equal to local address" in {
      val networkController = TestProbe()
      val remoteConnectionTestProbe: TestProbe = TestProbe()
      val peersKeeper: TestActorRef[PeersKeeper] = TestActorRef[PeersKeeper](PeersKeeper.props(testNetSettings, TestProbe().ref, TestProbe().ref))

      networkController.send(peersKeeper, VerifyConnection(
        new InetSocketAddress("0.0.0.0", 9001), remoteConnectionTestProbe.ref))
      networkController.expectNoMsg()
      peersKeeper.stop()
    }
    "handle outgoing connection" in {
      val networkController = TestProbe()
      val remoteConnectionTestProbe: TestProbe = TestProbe()
      val peersKeeper: TestActorRef[PeersKeeper] = TestActorRef[PeersKeeper](PeersKeeper.props(knowPeersSettings, TestProbe().ref, TestProbe().ref))

      peersKeeper ! RequestPeerForConnection
      peersKeeper.underlyingActor.outgoingConnections.contains(knowPeersSettings.network.knownPeers.head) shouldBe true
      networkController.send(peersKeeper, VerifyConnection(knowPeersSettings.network.knownPeers.head, remoteConnectionTestProbe.ref))
      networkController.expectMsg(
        ConnectionVerified(knowPeersSettings.network.knownPeers.head, remoteConnectionTestProbe.ref, Outgoing))
    }
  }
}