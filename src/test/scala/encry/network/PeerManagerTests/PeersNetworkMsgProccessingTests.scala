package encry.network.PeerManagerTests

import java.net.InetSocketAddress
import akka.actor.ActorSystem
import akka.testkit.TestProbe
import encry.modifiers.InstanceFactory
import encry.network.BasicMessagesRepo.Handshake
import encry.network.DeliveryManagerTests.DummyEncryAppSettingsReader
import encry.network.PeerConnectionHandler.{ConnectedPeer, Incoming}
import encry.network.PeerManager.ReceivableMessages.Handshaked
import encry.network.PeerManagerUtils
import encry.settings.EncryAppSettings
import encry.utils.NetworkTimeProvider
import org.scalatest.{BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike}

class PeersNetworkMsgProccessingTests extends WordSpecLike with BeforeAndAfterAll
  with Matchers
  with InstanceFactory
  with OneInstancePerTest {

  implicit val system: ActorSystem = ActorSystem("PeersNetworkMsgProccessingTests")

  "PeerManager" should {

    val dummyNVS = TestProbe("NVS")
    val settings: EncryAppSettings = DummyEncryAppSettingsReader.read
    val timeProvider: NetworkTimeProvider = new NetworkTimeProvider(settings.ntp)
    val peerManager = PeerManagerUtils.initPeerManager(dummyNVS.ref, settings, timeProvider)

    "add peers by theirs declared address" in {

      val firstAddressDeclared = new InetSocketAddress("111.11.11.11", 1001)
      val secondAddressDeclared = new InetSocketAddress("112.12.12.12", 1001)
      val thirdAddressDeclared = new InetSocketAddress("113.13.13.13", 1001)

      val peers = Seq(firstAddressDeclared, secondAddressDeclared, thirdAddressDeclared)

      val firstHandshake = Handshake(
        protocolToBytes(settings.network.appVersion),
        "dummyNode1",
        Some(firstAddressDeclared),
        0L
      )
      val secondHandshake = Handshake(
        protocolToBytes(settings.network.appVersion),
        "dummyNode1",
        Some(secondAddressDeclared),
        0L
      )
      val thirdHandshake = Handshake(
        protocolToBytes(settings.network.appVersion),
        "dummyNode1",
        Some(thirdAddressDeclared),
        0L
      )

      val firstConnectedPeer = ConnectedPeer(
        new InetSocketAddress("11.11.11.11", 1001),
        TestProbe("cp1").ref,
        Incoming,
        firstHandshake
      )
      val secondConnectedPeer = ConnectedPeer(
        new InetSocketAddress("12.12.12.12", 1001),
        TestProbe("cp2").ref,
        Incoming,
        secondHandshake
      )
      val thirdConnectedPeer = ConnectedPeer(
        new InetSocketAddress("13.13.13.13", 1001),
        TestProbe("cp3").ref,
        Incoming,
        thirdHandshake
      )

      peerManager ! Handshaked(firstConnectedPeer)
      peerManager ! Handshaked(secondConnectedPeer)
      peerManager ! Handshaked(thirdConnectedPeer)

      Thread.sleep(1000)

      peerManager.underlyingActor.connectedPeers.keys.forall(peers.contains) shouldBe true

      peerManager.underlyingActor.connectedPeers.size shouldEqual 3
    }
  }
}