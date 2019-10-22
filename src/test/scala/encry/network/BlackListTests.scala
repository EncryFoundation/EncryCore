package encry.network

import java.net.{InetAddress, InetSocketAddress}

import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestProbe}
import encry.network.BlackList.BanReason._
import encry.network.PeerConnectionHandler.{ConnectedPeer, Outgoing}
import encry.network.PeerConnectionHandler.ReceivableMessages.CloseConnection
import encry.network.PeersKeeper.BanPeer
import encry.settings.TestNetSettings
import org.encryfoundation.common.network.BasicMessagesRepo.Handshake
import org.scalatest.{BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike}
import encry.utils.Utils.protocolToBytes
import scala.concurrent.duration._

class BlackListTests extends WordSpecLike
  with Matchers
  with BeforeAndAfterAll
  with OneInstancePerTest
  with TestNetSettings {

  implicit val system: ActorSystem = ActorSystem()

  override def afterAll(): Unit = system.terminate()

  val knowPeersSettings = testNetSettings.copy(
    network = testNetSettings.network.copy(
      knownPeers = Seq(new InetSocketAddress("172.16.11.11", 9001)),
      connectOnlyWithKnownPeers = Some(true)
    ),
    blackList = testNetSettings.blackList.copy(
      banTime = 2 seconds,
      cleanupTime = 3 seconds
    ))

  /*
    Unit tests
   */
  "Black list" should {
    "temporary ban requested peer correctly" in {
      val blackList: BlackList = BlackList(knowPeersSettings)
      val peer: InetAddress = new InetSocketAddress("0.0.0.0", 9000).getAddress
      val newBL = blackList.banPeer(SemanticallyInvalidPersistentModifier, peer)
      newBL.contains(peer) shouldBe true
    }
    "clean black list from peers with expired ban time which were banned by temporary ban" in {
      val blackList: BlackList = BlackList(knowPeersSettings)
      val peer: InetAddress = new InetSocketAddress("0.0.0.0", 9000).getAddress
      val newBL = blackList.banPeer(SyntacticallyInvalidPersistentModifier, peer)
      Thread.sleep(2000)
      val newBL1 = newBL.cleanupBlackList
      newBL1.contains(peer) shouldBe false
    }
    "don't remove peer from black list before ban time expired" in {
      val blackList: BlackList = BlackList(knowPeersSettings)
      val peer: InetAddress = new InetSocketAddress("0.0.0.0", 9000).getAddress
      val newBL = blackList.banPeer(SentInvForPayload, peer)
      val newBL1 = newBL.cleanupBlackList
      newBL1.contains(peer) shouldBe true
    }
  }

  /*
    Akka tests
   */
  "Peers keeper" should {
    "handle ban peer message correctly" in {
      val peersKeeper: TestActorRef[PeersKeeper] = TestActorRef[PeersKeeper](PeersKeeper.props(knowPeersSettings, TestProbe().ref, TestProbe().ref))
      val address: InetSocketAddress = new InetSocketAddress("0.0.0.0", 9000)
      val peerHandler: TestProbe = TestProbe()
      val connectedPeer: ConnectedPeer = ConnectedPeer(
        address,
        peerHandler.ref,
        Outgoing,
        Handshake(protocolToBytes(knowPeersSettings.network.appVersion), "test node", Some(address), System.currentTimeMillis())
      )
      peersKeeper ! BanPeer(connectedPeer, SpamSender)
      peerHandler.expectMsg(CloseConnection)
      peersKeeper.underlyingActor.blackList.contains(address.getAddress) shouldBe true
    }
    "cleanup black list by scheduler correctly" in {
      val peersKeeper: TestActorRef[PeersKeeper] = TestActorRef[PeersKeeper](PeersKeeper.props(knowPeersSettings, TestProbe().ref, TestProbe().ref))
      val address: InetSocketAddress = new InetSocketAddress("0.0.0.0", 9000)
      val peerHandler: TestProbe = TestProbe()
      val connectedPeer: ConnectedPeer = ConnectedPeer(
        address,
        peerHandler.ref,
        Outgoing,
        Handshake(protocolToBytes(knowPeersSettings.network.appVersion), "test node", Some(address), System.currentTimeMillis())
      )
      peersKeeper ! BanPeer(connectedPeer, SentPeersMessageWithoutRequest)
      Thread.sleep(6000)
      peersKeeper.underlyingActor.blackList.contains(address.getAddress) shouldBe false
    }
    "don't remove peer from black list before ban time expired" in {
      val peersKeeper: TestActorRef[PeersKeeper] = TestActorRef[PeersKeeper](PeersKeeper.props(knowPeersSettings, TestProbe().ref, TestProbe().ref))
      val address: InetSocketAddress = new InetSocketAddress("0.0.0.0", 9000)
      val peerHandler: TestProbe = TestProbe()
      val connectedPeer: ConnectedPeer = ConnectedPeer(
        address,
        peerHandler.ref,
        Outgoing,
        Handshake(protocolToBytes(knowPeersSettings.network.appVersion), "test node", Some(address), System.currentTimeMillis())
      )
      Thread.sleep(4000)
      peersKeeper ! BanPeer(connectedPeer, CorruptedSerializedBytes)
      Thread.sleep(2000)
      peersKeeper.underlyingActor.blackList.contains(address.getAddress) shouldBe true
    }
  }
}