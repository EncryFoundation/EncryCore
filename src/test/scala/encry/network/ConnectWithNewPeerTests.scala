package encry.network

import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestActorRef, TestProbe}
import encry.api.http.DataHolderForApi.UpdatingPeersInfo
import encry.modifiers.InstanceFactory
import encry.network.BlackList.BanReason._
import encry.network.NetworkController.ReceivableMessages.{DataFromPeer, RegisterMessagesHandler}
import encry.network.PeerConnectionHandler.ReceivableMessages.CloseConnection
import encry.network.PeerConnectionHandler.{ConnectedPeer, Incoming, Outgoing}
import encry.network.PeersKeeper.ConnectionStatusMessages.{ConnectionStopped, ConnectionVerified, HandshakedDone, NewConnection, OutgoingConnectionFailed}
import encry.network.PeersKeeper._
import encry.settings.TestNetSettings
import org.encryfoundation.common.network.BasicMessagesRepo.{GetPeersNetworkMessage, Handshake, PeersNetworkMessage}
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
        "maintain outgoing connection process correctly" in {
          /* Request first peer while current number of connections is 0 */
          val networkController: TestProbe = TestProbe()
          val peersKeeper: TestActorRef[PK] = TestActorRef[PK](
            PK.props(settings.network.copy(maxConnections = 2), settings.blackList),
            networkController.ref
          )

          val availablePeers: Set[InetSocketAddress] = peersKeeper.underlyingActor.knownPeers

          networkController.send(peersKeeper, RequestPeerForConnection)
          println(peersKeeper.underlyingActor.knownPeers.map(PeerForConnection))
          networkController.expectMsg(RegisterMessagesHandler(Seq(
            PeersNetworkMessage.NetworkMessageTypeID -> "PeersNetworkMessage",
            GetPeersNetworkMessage.NetworkMessageTypeID -> "GetPeersNetworkMessage"
          ), peersKeeper.underlying.self))
          val msg = networkController.expectMsgType[PeerForConnection]
          peersKeeper.underlyingActor.outgoingConnections.contains(msg.peer) shouldBe true
          peersKeeper.underlyingActor.awaitingHandshakeConnections.contains(msg.peer) shouldBe true

          val remoteAkkaConnectionHandler = TestProbe()

          networkController.send(peersKeeper, NewConnection(msg.peer, remoteAkkaConnectionHandler.ref))
          networkController.expectMsg(ConnectionVerified(msg.peer, remoteAkkaConnectionHandler.ref, Outgoing))
          peersKeeper.underlyingActor.outgoingConnections.contains(msg.peer) shouldBe false
          peersKeeper.underlyingActor.awaitingHandshakeConnections.contains(msg.peer) shouldBe true

          val peerHandler = TestProbe()
          val connectedPeer: ConnectedPeer = ConnectedPeer(availablePeers.head, peerHandler.ref, Outgoing,
            Handshake(protocolToBytes(settings.network.appVersion),
              "test-peer", Some(availablePeers.head), System.currentTimeMillis()))

          networkController.send(peersKeeper, HandshakedDone(connectedPeer))
          peersKeeper.underlyingActor.awaitingHandshakeConnections.contains(availablePeers.head) shouldBe false
          peersKeeper.underlyingActor.knownPeers.contains(availablePeers.head) shouldBe true
          networkController.expectMsgType[UpdatingPeersInfo]

          /* Request next peer after first connection setup */

          val newAvailablePeers: Set[InetSocketAddress] = peersKeeper.underlyingActor.knownPeers.drop(1)

          networkController.send(peersKeeper, RequestPeerForConnection)
          val nextPeerForConnectionMsg = networkController.expectMsgType[PeerForConnection]
          peersKeeper.underlyingActor.outgoingConnections.contains(nextPeerForConnectionMsg.peer) shouldBe true
          peersKeeper.underlyingActor.awaitingHandshakeConnections.contains(nextPeerForConnectionMsg.peer) shouldBe true

          networkController.send(peersKeeper, NewConnection(newAvailablePeers.filter(_ != nextPeerForConnectionMsg.peer).head, remoteAkkaConnectionHandler.ref))
          networkController.expectMsg(ConnectionVerified(newAvailablePeers.filter(_ != nextPeerForConnectionMsg.peer).head, remoteAkkaConnectionHandler.ref, Incoming))
          peersKeeper.underlyingActor.outgoingConnections.contains(newAvailablePeers.filter(_ != nextPeerForConnectionMsg.peer).head) shouldBe false
          peersKeeper.underlyingActor.awaitingHandshakeConnections.contains(newAvailablePeers.filter(_ != nextPeerForConnectionMsg.peer).head) shouldBe true

          val newPeerHandler = TestProbe()
          val newConnectedPeer: ConnectedPeer = ConnectedPeer(newAvailablePeers.filter(_ != nextPeerForConnectionMsg.peer).head, newPeerHandler.ref, Outgoing,
            Handshake(protocolToBytes(settings.network.appVersion),
              "test-peer_new", Some(newAvailablePeers.filter(_ != nextPeerForConnectionMsg.peer).head), System.currentTimeMillis()))

          networkController.send(peersKeeper, HandshakedDone(newConnectedPeer))
          peersKeeper.underlyingActor.awaitingHandshakeConnections.contains(newAvailablePeers.filter(_ != nextPeerForConnectionMsg.peer).head) shouldBe false
          peersKeeper.underlyingActor.knownPeers.contains(newAvailablePeers.filter(_ != nextPeerForConnectionMsg.peer).head) shouldBe true
          networkController.expectMsgType[UpdatingPeersInfo]
          /* Try to ask one more peer while max number of connections has been expired */

          networkController.send(peersKeeper, RequestPeerForConnection)
          networkController.expectNoMsg()

          /* Now we will ban one peer */

          val actorWhichSendBanMessage = TestProbe()

          actorWhichSendBanMessage.send(peersKeeper, BanPeer(newConnectedPeer.socketAddress, ExpiredNumberOfConnections))
          newPeerHandler.expectMsgAnyOf(CloseConnection)
          peersKeeper.underlyingActor.blackList.contains(newConnectedPeer.socketAddress.getAddress) shouldBe true
          networkController.send(peersKeeper, ConnectionStopped(newConnectedPeer.socketAddress))
          peersKeeper.underlyingActor.connectedPeers.contains(newConnectedPeer.socketAddress) shouldBe false
          networkController.expectMsgType[UpdatingPeersInfo]

          /* Try to setup Incoming connection from banned peer */

          networkController.send(peersKeeper, NewConnection(newConnectedPeer.socketAddress, remoteAkkaConnectionHandler.ref))
          networkController.expectNoMsg()

          /* Try to request new connection */

          networkController.send(peersKeeper, RequestPeerForConnection)
          val oneMorePeer = networkController.expectMsgType[PeerForConnection]
          peersKeeper.underlyingActor.outgoingConnections.contains(oneMorePeer.peer) shouldBe true
          peersKeeper.underlyingActor.awaitingHandshakeConnections.contains(oneMorePeer.peer) shouldBe true

          val updatedRemoteAkkaConnectionHandler = TestProbe()

          networkController.send(peersKeeper, NewConnection(oneMorePeer.peer, updatedRemoteAkkaConnectionHandler.ref))
          networkController.expectMsg(ConnectionVerified(oneMorePeer.peer, updatedRemoteAkkaConnectionHandler.ref, Outgoing))
          peersKeeper.underlyingActor.outgoingConnections.contains(oneMorePeer.peer) shouldBe false
          peersKeeper.underlyingActor.awaitingHandshakeConnections.contains(oneMorePeer.peer) shouldBe true

          val updatedConnectedPeer: ConnectedPeer = ConnectedPeer(oneMorePeer.peer, updatedRemoteAkkaConnectionHandler.ref, Outgoing,
            Handshake(protocolToBytes(settings.network.appVersion),
              "test-peer", Some(oneMorePeer.peer), System.currentTimeMillis()))

          networkController.send(peersKeeper, HandshakedDone(updatedConnectedPeer))
          peersKeeper.underlyingActor.awaitingHandshakeConnections.contains(oneMorePeer.peer) shouldBe false
          peersKeeper.underlyingActor.knownPeers.contains(oneMorePeer.peer) shouldBe true
        }
        "remove peer from available we can't connect to" in {
          val networkController: TestProbe = TestProbe()
          val peersKeeper: TestActorRef[PK] = TestActorRef[PK](
            PK.props(settings.network.copy(maxConnections = 2, knownPeers = List(new InetSocketAddress("1.1.1.1", 1234))), settings.blackList),
            networkController.ref
          )
          networkController.expectMsg(RegisterMessagesHandler(Seq(
            PeersNetworkMessage.NetworkMessageTypeID -> "PeersNetworkMessage",
            GetPeersNetworkMessage.NetworkMessageTypeID -> "GetPeersNetworkMessage"
          ), peersKeeper.underlying.self))

          val availablePeers: Set[InetSocketAddress] = peersKeeper.underlyingActor.knownPeers

          networkController.send(peersKeeper, RequestPeerForConnection)
          val msg = networkController.expectMsgType[PeerForConnection]

          networkController.send(peersKeeper, OutgoingConnectionFailed(msg.peer))
          peersKeeper.underlyingActor.outgoingConnections.contains(msg.peer) shouldBe false
          peersKeeper.underlyingActor.awaitingHandshakeConnections.contains(msg.peer) shouldBe false

          networkController.send(peersKeeper, RequestPeerForConnection)
          val nextPeer = networkController.expectMsgType[PeerForConnection]

          networkController.send(peersKeeper, OutgoingConnectionFailed(nextPeer.peer))
          peersKeeper.underlyingActor.outgoingConnections.contains(nextPeer.peer) shouldBe false
          peersKeeper.underlyingActor.awaitingHandshakeConnections.contains(nextPeer.peer) shouldBe false
        }
    "remove peer from available if it has been banned" in {
      val networkController: TestProbe = TestProbe()
      val peersKeeper: TestActorRef[PK] = TestActorRef[PK](
        PK.props(settings.network.copy(maxConnections = 2, knownPeers = List(new InetSocketAddress("1.1.1.1", 1234))), settings.blackList),
        networkController.ref
      )
      networkController.expectMsg(RegisterMessagesHandler(Seq(
        PeersNetworkMessage.NetworkMessageTypeID -> "PeersNetworkMessage",
        GetPeersNetworkMessage.NetworkMessageTypeID -> "GetPeersNetworkMessage"
      ), peersKeeper.underlying.self))

      val availablePeers: Map[InetSocketAddress, Int] = peersKeeper.underlyingActor.peersForConnection

      val peerHandler = TestProbe()
      val connectedPeer: ConnectedPeer = ConnectedPeer(availablePeers.head._1, peerHandler.ref, Outgoing,
        Handshake(protocolToBytes(testNetSettings.network.appVersion),
          "test-peer", Some(availablePeers.head._1), System.currentTimeMillis()))

      networkController.send(peersKeeper, BanPeer(connectedPeer.socketAddress, ExpiredNumberOfConnections))
      networkController.send(peersKeeper, ConnectionStopped(availablePeers.head._1))

      peersKeeper.underlyingActor.peersForConnection.contains(availablePeers.head._1) shouldBe false
    }
    "filter peers from network message" in {
      val networkController: TestProbe = TestProbe()
      val peersKeeper: TestActorRef[PK] = TestActorRef[PK](
        PK.props(settings.network.copy(maxConnections = 2), settings.blackList),
        networkController.ref
      )
      networkController.expectMsg(RegisterMessagesHandler(Seq(
        PeersNetworkMessage.NetworkMessageTypeID -> "PeersNetworkMessage",
        GetPeersNetworkMessage.NetworkMessageTypeID -> "GetPeersNetworkMessage"
      ), peersKeeper.underlying.self))

      val availablePeers: Map[InetSocketAddress, Int] = peersKeeper.underlyingActor.peersForConnection

      val peerHandler = TestProbe()
      val connectedPeer: ConnectedPeer = ConnectedPeer(availablePeers.head._1, peerHandler.ref, Outgoing,
        Handshake(protocolToBytes(testNetSettings.network.appVersion),
          "test-peer", Some(availablePeers.head._1), System.currentTimeMillis()))

      val newPeerHandler = TestProbe()
      val newConnectedPeer: ConnectedPeer = ConnectedPeer(availablePeers.last._1, newPeerHandler.ref, Outgoing,
        Handshake(protocolToBytes(testNetSettings.network.appVersion),
          "test-peer", Some(availablePeers.last._1), System.currentTimeMillis()))

      networkController.send(peersKeeper, BanPeer(connectedPeer.socketAddress, ExpiredNumberOfConnections))
      networkController.send(peersKeeper, ConnectionStopped(availablePeers.head._1))
      networkController.send(peersKeeper, HandshakedDone(newConnectedPeer))
      peersKeeper.underlyingActor.peersForConnection.contains(availablePeers.head._1) shouldBe false

      val peer = new InetSocketAddress("172.16.28.98", 9023)
      val peers = Seq(availablePeers.last._1, availablePeers.head._1, peer)

      networkController.send(peersKeeper, DataFromPeer(PeersNetworkMessage(peers), newConnectedPeer.socketAddress))
      peersKeeper.underlyingActor.peersForConnection.contains(availablePeers.head._1) shouldBe false
      peersKeeper.underlyingActor.peersForConnection.contains(peer) shouldBe true
    }
    "handle successful connection process" in {
      val networkController: TestProbe = TestProbe()
      val peerAddr = new InetSocketAddress("1.1.1.1", 1234)
      val peersKeeper: TestActorRef[PK] = TestActorRef[PK](
        PK.props(settings.network.copy(maxConnections = 2, knownPeers = List(peerAddr)), settings.blackList),
        networkController.ref
      )
      networkController.expectMsg(RegisterMessagesHandler(Seq(
        PeersNetworkMessage.NetworkMessageTypeID -> "PeersNetworkMessage",
        GetPeersNetworkMessage.NetworkMessageTypeID -> "GetPeersNetworkMessage"
      ), peersKeeper.underlying.self))

      val peerHandler = TestProbe()
      val connectedPeer: ConnectedPeer = ConnectedPeer(peerAddr, peerHandler.ref, Outgoing,
        Handshake(protocolToBytes(testNetSettings.network.appVersion),
          "test-peer", Some(peerAddr), System.currentTimeMillis()))

      networkController.send(peersKeeper, RequestPeerForConnection)
      networkController.expectMsg(PeerForConnection(peerAddr))
      peersKeeper.underlyingActor.outgoingConnections.contains(peerAddr) shouldBe true
      peersKeeper.underlyingActor.peersForConnection.contains(peerAddr) shouldBe true

      networkController.send(peersKeeper, NewConnection(peerAddr, peerHandler.ref))
      networkController.expectMsg(
        ConnectionVerified(peerAddr, peerHandler.ref, Outgoing))

      networkController.send(peersKeeper, HandshakedDone(connectedPeer))
      peersKeeper.underlyingActor.connectedPeers.contains(peerAddr) shouldBe true
    }
    "handle stop connection process" in {
      val networkController: TestProbe = TestProbe()
      val peerAddr = new InetSocketAddress("1.1.1.1", 1234)
      val peersKeeper: TestActorRef[PK] = TestActorRef[PK](
        PK.props(settings.network.copy(maxConnections = 2, knownPeers = List(peerAddr)), settings.blackList),
        networkController.ref
      )
      networkController.expectMsg(RegisterMessagesHandler(Seq(
        PeersNetworkMessage.NetworkMessageTypeID -> "PeersNetworkMessage",
        GetPeersNetworkMessage.NetworkMessageTypeID -> "GetPeersNetworkMessage"
      ), peersKeeper.underlying.self))
      val peersSenderProbe = TestProbe()

      networkController.send(peersKeeper, RequestPeerForConnection)
      val msg = networkController.expectMsgType[PeerForConnection]
      val connectedPeer: ConnectedPeer = ConnectedPeer(msg.peer, peersSenderProbe.ref, Outgoing,
        Handshake(protocolToBytes(testNetSettings.network.appVersion),
          "test-peer", Some(msg.peer), System.currentTimeMillis()))
      peersKeeper.underlyingActor.outgoingConnections.contains(msg.peer) shouldBe true
      peersKeeper.underlyingActor.peersForConnection.contains(msg.peer) shouldBe true

      networkController.send(peersKeeper, NewConnection(msg.peer, peersSenderProbe.ref))
      networkController.expectMsg(ConnectionVerified(msg.peer, peersSenderProbe.ref, Outgoing))

      networkController.send(peersKeeper, HandshakedDone(connectedPeer))
      peersKeeper.underlyingActor.connectedPeers.contains(msg.peer) shouldBe true

      peersKeeper ! ConnectionStopped(msg.peer)
      peersKeeper.underlyingActor.connectedPeers.contains(msg.peer) shouldBe false
      peersKeeper.underlyingActor.peersForConnection.contains(msg.peer) shouldBe true
    }
//    "handle failed connection process" in {
//      val networkController = TestProbe()
//      val peersSenderProbe = TestProbe()
//      val peersKeeper: TestActorRef[PeersKeeper] = TestActorRef[PeersKeeper](PeersKeeper.props(knowPeersSettings, TestProbe().ref, TestProbe().ref))
//
//      networkController.send(peersKeeper, RequestPeerForConnection)
//      networkController.expectMsg(PeerForConnection(testNetSettings.network.knownPeers.head))
//      peersKeeper.underlyingActor.outgoingConnections.contains(testNetSettings.network.knownPeers.head) shouldBe true
//      peersKeeper.underlyingActor.peersForConnection.contains(testNetSettings.network.knownPeers.head) shouldBe true
//
//      networkController.send(peersKeeper, NewConnection(testNetSettings.network.knownPeers.head, peersSenderProbe.ref))
//      networkController.expectMsg(
//        ConnectionVerified(testNetSettings.network.knownPeers.head, peersSenderProbe.ref, Outgoing))
//
//      peersKeeper ! OutgoingConnectionFailed(testNetSettings.network.knownPeers.head)
//      peersKeeper.underlyingActor.outgoingConnections.contains(testNetSettings.network.knownPeers.head) shouldBe false
//      peersKeeper.underlyingActor.peersForConnection.contains(testNetSettings.network.knownPeers.head) shouldBe true
//    }
//    "handle incoming connections correctly while connection with only known peers false " +
//      "and incoming peer doesn't contains in black list and connected peers collection" in {
//      val networkController = TestProbe()
//      val remoteConnectionTestProbe: TestProbe = TestProbe()
//      val remoteAddress: InetSocketAddress = testNetSettings.network.knownPeers.head
//      val peersKeeper: TestActorRef[PeersKeeper] = TestActorRef[PeersKeeper](PeersKeeper.props(testNetSettings, TestProbe().ref, TestProbe().ref))
//
//      networkController.send(peersKeeper, NewConnection(remoteAddress, remoteConnectionTestProbe.ref))
//      networkController.expectMsg(ConnectionVerified(remoteAddress, remoteConnectionTestProbe.ref, Incoming))
//      peersKeeper.stop()
//    }
//    "handle incoming connections correctly while connection with only known peers false " +
//      "and incoming peer contain in black list" in {
//      val networkController = TestProbe()
//      val remoteConnectionTestProbe: TestProbe = TestProbe()
//      val remoteAddress: InetSocketAddress = new InetSocketAddress("172.16.11.11", 9001)
//      val peersKeeper: TestActorRef[PeersKeeper] = TestActorRef[PeersKeeper](PeersKeeper.props(testNetSettings, TestProbe().ref, TestProbe().ref))
//      val connectedPeer: ConnectedPeer = ConnectedPeer(remoteAddress, remoteConnectionTestProbe.ref, Incoming,
//        Handshake(protocolToBytes(testNetSettings.network.appVersion),
//          "test-peer", Some(remoteAddress), System.currentTimeMillis()))
//
//      peersKeeper ! BanPeer(connectedPeer, SyntacticallyInvalidPersistentModifier)
//      networkController.send(peersKeeper, NewConnection(remoteAddress, remoteConnectionTestProbe.ref))
//      networkController.expectNoMsg()
//      peersKeeper.stop()
//    }
//    "handle incoming connections correctly while connection with only known peers false " +
//      "and incoming peer contain in connected peers" in {
//      val networkController = TestProbe()
//      val remoteConnectionTestProbe: TestProbe = TestProbe()
//      val remoteAddress: InetSocketAddress = new InetSocketAddress("172.16.11.11", 9001)
//      val peersKeeper: TestActorRef[PeersKeeper] = TestActorRef[PeersKeeper](PeersKeeper.props(testNetSettings, TestProbe().ref, TestProbe().ref))
//      val connectedPeer: ConnectedPeer = ConnectedPeer(remoteAddress, remoteConnectionTestProbe.ref, Incoming,
//        Handshake(protocolToBytes(testNetSettings.network.appVersion),
//          "test-peer", Some(remoteAddress), System.currentTimeMillis()))
//
//      peersKeeper ! HandshakedDone(connectedPeer)
//      networkController.send(peersKeeper, NewConnection(remoteAddress, remoteConnectionTestProbe.ref))
//      networkController.expectNoMsg()
//      peersKeeper.stop()
//    }
//    "handle incoming connections correctly while connection with only known peers true" in {
//      val networkController = TestProbe()
//      val remoteConnectionTestProbe: TestProbe = TestProbe()
//      val remoteAddress: InetSocketAddress = new InetSocketAddress("172.16.11.99", 9001)
//      val peersKeeper: TestActorRef[PeersKeeper] = TestActorRef[PeersKeeper](PeersKeeper.props(knowPeersSettings, TestProbe().ref, TestProbe().ref))
//
//      networkController.send(peersKeeper, NewConnection(remoteAddress, remoteConnectionTestProbe.ref))
//      networkController.expectNoMsg()
//      peersKeeper.stop()
//    }
//    "handle incoming connections correctly while peer is equal to local address" in {
//      val networkController = TestProbe()
//      val remoteConnectionTestProbe: TestProbe = TestProbe()
//      val peersKeeper: TestActorRef[PeersKeeper] = TestActorRef[PeersKeeper](PeersKeeper.props(testNetSettings, TestProbe().ref, TestProbe().ref))
//
//      networkController.send(peersKeeper, NewConnection(
//        new InetSocketAddress("0.0.0.0", 9001), remoteConnectionTestProbe.ref))
//      networkController.expectNoMsg()
//      peersKeeper.stop()
//    }
//    "handle outgoing connection" in {
//      val networkController = TestProbe()
//      val remoteConnectionTestProbe: TestProbe = TestProbe()
//      val peersKeeper: TestActorRef[PeersKeeper] = TestActorRef[PeersKeeper](PeersKeeper.props(knowPeersSettings, TestProbe().ref, TestProbe().ref))
//
//      peersKeeper ! RequestPeerForConnection
//      peersKeeper.underlyingActor.outgoingConnections.contains(knowPeersSettings.network.knownPeers.head) shouldBe true
//      networkController.send(peersKeeper, NewConnection(knowPeersSettings.network.knownPeers.head, remoteConnectionTestProbe.ref))
//      networkController.expectMsg(
//        ConnectionVerified(knowPeersSettings.network.knownPeers.head, remoteConnectionTestProbe.ref, Outgoing))
//    }
  }
}