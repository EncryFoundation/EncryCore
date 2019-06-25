package encry.network

import java.net.InetSocketAddress
import akka.actor.ActorSystem
import akka.testkit.TestProbe
import encry.consensus.History.{Fork, Older, Unknown, Younger}
import encry.modifiers.InstanceFactory
import encry.network.ConnectedPeersCollection.PeerInfo
import encry.network.PeerConnectionHandler.{ConnectedPeer, Outgoing}
import encry.network.PrioritiesCalculator.PeersPriorityStatus.PeersPriorityStatus._
import encry.settings.EncryAppSettings
import org.encryfoundation.common.network.BasicMessagesRepo.Handshake
import org.scalatest.{Matchers, OneInstancePerTest, WordSpecLike}

class ConnectedPeersCollectionsTests extends WordSpecLike
  with Matchers
  with InstanceFactory
  with OneInstancePerTest {

  val settingsWithKnownPeers: EncryAppSettings = NetworkUtils.TestNetworkSettings.read("AdditionalTestSettings.conf")
  implicit val system: ActorSystem = ActorSystem()


  "ConnectedPeersCollection" should {
    "initialize new peer" in {
      val connectedPeersCollection = ConnectedPeersCollection()

      val address: InetSocketAddress = new InetSocketAddress("0.0.0.0", 9000)
      val peerHandler: TestProbe = TestProbe()
      val connectedPeer: ConnectedPeer = ConnectedPeer(
        address,
        peerHandler.ref,
        Outgoing,
        Handshake(protocolToBytes(settingsWithKnownPeers.network.appVersion), "test node", Some(address), System.currentTimeMillis())
      )

      val address1: InetSocketAddress = new InetSocketAddress("0.0.0.1", 9000)
      val peerHandler1: TestProbe = TestProbe()
      val connectedPeer1: ConnectedPeer = ConnectedPeer(
        address1,
        peerHandler1.ref,
        Outgoing,
        Handshake(protocolToBytes(settingsWithKnownPeers.network.appVersion), "test node 1", Some(address1), System.currentTimeMillis())
      )

      val peersSeq = Seq(connectedPeer, connectedPeer1)

      val cpc = peersSeq.foldLeft(connectedPeersCollection) { case (cpcl, p) =>
        val newCPC = cpcl.initializePeer(p)
        newCPC.contains(p.socketAddress) shouldBe true
        newCPC
      }
    }
    "update priority status" in {
      val connectedPeersCollection = ConnectedPeersCollection()

      val address: InetSocketAddress = new InetSocketAddress("0.0.0.0", 9000)
      val peerHandler: TestProbe = TestProbe()
      val connectedPeer: ConnectedPeer = ConnectedPeer(
        address,
        peerHandler.ref,
        Outgoing,
        Handshake(protocolToBytes(settingsWithKnownPeers.network.appVersion), "test node", Some(address), System.currentTimeMillis())
      )

      val address1: InetSocketAddress = new InetSocketAddress("0.0.0.1", 9000)
      val peerHandler1: TestProbe = TestProbe()
      val connectedPeer1: ConnectedPeer = ConnectedPeer(
        address1,
        peerHandler1.ref,
        Outgoing,
        Handshake(protocolToBytes(settingsWithKnownPeers.network.appVersion), "test node 1", Some(address1), System.currentTimeMillis())
      )

      val peersSeq = Seq(connectedPeer, connectedPeer1)

      val cpc = peersSeq.foldLeft(connectedPeersCollection) { case (cpcl, p) =>
        val newCPC = cpcl.initializePeer(p)
        newCPC.contains(p.socketAddress) shouldBe true
        newCPC
      }

      val address2 = new InetSocketAddress("1.1.1.1", 9019)

      val priorityStatus = Map(address -> BadNode, address1 -> HighPriority, address2 -> LowPriority)

      val newCpc = cpc.updatePriorityStatus(priorityStatus)
      val cpcState = newCpc.collect((_, _) => true, (a: InetSocketAddress, b: PeerInfo) => a -> b).toMap
      cpcState(address).peerPriorityStatus shouldBe BadNode
      cpcState(address1).peerPriorityStatus shouldBe HighPriority
      cpcState.get(address2) shouldBe None
    }
    "update history comparison result" in {
      val connectedPeersCollection = ConnectedPeersCollection()

      val address: InetSocketAddress = new InetSocketAddress("0.0.0.0", 9000)
      val peerHandler: TestProbe = TestProbe()
      val connectedPeer: ConnectedPeer = ConnectedPeer(
        address,
        peerHandler.ref,
        Outgoing,
        Handshake(protocolToBytes(settingsWithKnownPeers.network.appVersion), "test node", Some(address), System.currentTimeMillis())
      )

      val address1: InetSocketAddress = new InetSocketAddress("0.0.0.1", 9000)
      val peerHandler1: TestProbe = TestProbe()
      val connectedPeer1: ConnectedPeer = ConnectedPeer(
        address1,
        peerHandler1.ref,
        Outgoing,
        Handshake(protocolToBytes(settingsWithKnownPeers.network.appVersion), "test node 1", Some(address1), System.currentTimeMillis())
      )

      val address3: InetSocketAddress = new InetSocketAddress("0.3.0.0", 9000)
      val peerHandler3: TestProbe = TestProbe()
      val connectedPeer3: ConnectedPeer = ConnectedPeer(
        address3,
        peerHandler3.ref,
        Outgoing,
        Handshake(protocolToBytes(settingsWithKnownPeers.network.appVersion), "test node3", Some(address3), System.currentTimeMillis())
      )

      val peersSeq = Seq(connectedPeer, connectedPeer1, connectedPeer3)

      val cpc = peersSeq.foldLeft(connectedPeersCollection) { case (cpcl, p) =>
        val newCPC = cpcl.initializePeer(p)
        newCPC.contains(p.socketAddress) shouldBe true
        newCPC
      }

      val address2 = new InetSocketAddress("1.1.1.1", 9019)

      val peersSeqN = Seq(address -> Older, address1 -> Younger, address2 -> Fork)

      val newCpc = peersSeqN.foldLeft(cpc) { case (coll, (p, hc)) =>
        coll.updateHistoryComparisonResult(Map(p -> hc))
      }
      val cpcState = newCpc.collect((_, _) => true, (a: InetSocketAddress, b: PeerInfo) => a -> b).toMap
      cpcState(address).historyComparisonResult shouldBe Older
      cpcState(address1).historyComparisonResult shouldBe Younger
      cpcState(address3).historyComparisonResult shouldBe Unknown
      cpcState.get(address2) shouldBe None
    }
    "remove peer" in {
      val connectedPeersCollection = ConnectedPeersCollection()

      val address: InetSocketAddress = new InetSocketAddress("0.0.0.0", 9000)
      val peerHandler: TestProbe = TestProbe()
      val connectedPeer: ConnectedPeer = ConnectedPeer(
        address,
        peerHandler.ref,
        Outgoing,
        Handshake(protocolToBytes(settingsWithKnownPeers.network.appVersion), "test node", Some(address), System.currentTimeMillis())
      )

      val address1: InetSocketAddress = new InetSocketAddress("0.0.0.1", 9000)
      val peerHandler1: TestProbe = TestProbe()
      val connectedPeer1: ConnectedPeer = ConnectedPeer(
        address1,
        peerHandler1.ref,
        Outgoing,
        Handshake(protocolToBytes(settingsWithKnownPeers.network.appVersion), "test node 1", Some(address1), System.currentTimeMillis())
      )

      val address3: InetSocketAddress = new InetSocketAddress("0.3.0.0", 9000)
      val peerHandler3: TestProbe = TestProbe()
      val connectedPeer3: ConnectedPeer = ConnectedPeer(
        address3,
        peerHandler3.ref,
        Outgoing,
        Handshake(protocolToBytes(settingsWithKnownPeers.network.appVersion), "test node3", Some(address3), System.currentTimeMillis())
      )

      val peersSeq = Seq(connectedPeer, connectedPeer1, connectedPeer3)

      val cpc = peersSeq.foldLeft(connectedPeersCollection) { case (cpcl, p) =>
        val newCPC = cpcl.initializePeer(p)
        newCPC.contains(p.socketAddress) shouldBe true
        newCPC
      }

      val newCpc = cpc.removePeer(address)
      val cpcState1 = newCpc.collect((_, _) => true, (a: InetSocketAddress, b: PeerInfo) => a -> b).toMap
      cpcState1.contains(address1) shouldBe true
      cpcState1.contains(address3) shouldBe true
      cpcState1.contains(address) shouldBe false

      val newCpc1 = newCpc.removePeer(address1)
      val cpcState2 = newCpc1.collect((_, _) => true, (a: InetSocketAddress, b: PeerInfo) => a -> b).toMap
      cpcState2.contains(address1) shouldBe false
      cpcState2.contains(address3) shouldBe true
      cpcState2.contains(address) shouldBe false

      val newCpc2 = newCpc1.removePeer(address3)
      val cpcState3 = newCpc2.collect((_, _) => true, (a: InetSocketAddress, b: PeerInfo) => a -> b).toMap
      cpcState3.contains(address1) shouldBe false
      cpcState3.contains(address3) shouldBe false
      cpcState3.contains(address) shouldBe false
      cpcState3 shouldBe Map.empty
    }
  }
}