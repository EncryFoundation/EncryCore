package encry.network

import java.net.{InetAddress, InetSocketAddress}

import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestProbe}
import encry.consensus.History
import encry.consensus.History.Older
import encry.local.miner.Miner.StartMining
import encry.network.BlackList._
import encry.modifiers.InstanceFactory
import encry.network.DeliveryManager.FullBlockChainIsSynced
import encry.network.DeliveryManagerTests.{DMUtils, DummyEncryAppSettingsReader}
import encry.network.NetworkController.ReceivableMessages.DataFromPeer
import encry.network.NodeViewSynchronizer.ReceivableMessages.{RequestFromLocal, UpdatedHistory}
import encry.network.PeerConnectionHandler.ReceivableMessages.CloseConnection
import encry.network.PeerConnectionHandler.{ConnectedPeer, Outgoing}
import encry.network.PeersKeeper.{BanPeer, RequestPeersForFirstSyncInfo, UpdatedPeersCollection}
import encry.network.PrioritiesCalculator.PeersPriorityStatus.InitialPriority
import encry.settings.EncryAppSettings
import encry.view.history.EncryHistory
import org.encryfoundation.common.crypto.equihash.EquihashSolution
import org.encryfoundation.common.modifiers.history.{Header, HeaderProtoSerializer}
import org.encryfoundation.common.network.BasicMessagesRepo.{Handshake, ModifiersNetworkMessage}
import org.encryfoundation.common.utils.TaggedTypes.{ADDigest, ModifierId}
import org.encryfoundation.common.utils.constants.TestNetConstants
import org.scalatest.{BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike}
import scorex.crypto.hash.Digest32
import scorex.utils.Random

class BlackListTests extends WordSpecLike
  with Matchers
  with BeforeAndAfterAll
  with InstanceFactory
  with OneInstancePerTest {

  val settingsWithKnownPeers: EncryAppSettings = NetworkUtils.TestNetworkSettings.read("AdditionalTestSettings.conf")
  implicit val system: ActorSystem = ActorSystem()

  override def afterAll(): Unit = system.terminate()

  /*
    Unit tests
   */
  "Black list" should {
    "temporary ban requested peer correctly" in {
      val blackList: BlackList = new BlackList(settingsWithKnownPeers)
      val peer: InetAddress = new InetSocketAddress("0.0.0.0", 9000).getAddress
      blackList.banPeer(SemanticallyInvalidModifier, peer)
      blackList.contains(peer) shouldBe true
    }
    "clean black list from peers with expired ban time which were banned by temporary ban" in {
      val blackList: BlackList = new BlackList(settingsWithKnownPeers)
      val peer: InetAddress = new InetSocketAddress("0.0.0.0", 9000).getAddress
      blackList.banPeer(SyntacticallyInvalidModifier, peer)
      Thread.sleep(2000)
      blackList.cleanupBlackList()
      blackList.contains(peer) shouldBe false
    }
    "don't remove peer from black list before ban time expired" in {
      val blackList: BlackList = new BlackList(settingsWithKnownPeers)
      val peer: InetAddress = new InetSocketAddress("0.0.0.0", 9000).getAddress
      blackList.banPeer(SentInvForPayload, peer)
      blackList.cleanupBlackList()
      blackList.contains(peer) shouldBe true
    }
  }

  /*
    Akka tests
   */
  "Peers keeper" should {
    "handle ban peer message correctly" in {
      val peersKeeper: TestActorRef[PeersKeeper] = TestActorRef[PeersKeeper](PeersKeeper.props(settingsWithKnownPeers, TestProbe().ref, TestProbe().ref))
      val address: InetSocketAddress = new InetSocketAddress("0.0.0.0", 9000)
      val peerHandler: TestProbe = TestProbe()
      val connectedPeer: ConnectedPeer = ConnectedPeer(
        address,
        peerHandler.ref,
        Outgoing,
        Handshake(protocolToBytes(settingsWithKnownPeers.network.appVersion), "test node", Some(address), System.currentTimeMillis())
      )
      peersKeeper ! BanPeer(connectedPeer, SpamSender)
      peerHandler.expectMsg(CloseConnection)
      peersKeeper.underlyingActor.blackList.contains(address.getAddress) shouldBe true
    }
    "cleanup black list by scheduler correctly" in {
      val peersKeeper: TestActorRef[PeersKeeper] = TestActorRef[PeersKeeper](PeersKeeper.props(settingsWithKnownPeers, TestProbe().ref, TestProbe().ref))
      val address: InetSocketAddress = new InetSocketAddress("0.0.0.0", 9000)
      val peerHandler: TestProbe = TestProbe()
      val connectedPeer: ConnectedPeer = ConnectedPeer(
        address,
        peerHandler.ref,
        Outgoing,
        Handshake(protocolToBytes(settingsWithKnownPeers.network.appVersion), "test node", Some(address), System.currentTimeMillis())
      )
      peersKeeper ! BanPeer(connectedPeer, SentPeersMessageWithoutRequest)
      Thread.sleep(6000)
      peersKeeper.underlyingActor.blackList.contains(address.getAddress) shouldBe false
    }
    "don't remove peer from black list before ban time expired" in {
      val peersKeeper: TestActorRef[PeersKeeper] = TestActorRef[PeersKeeper](PeersKeeper.props(settingsWithKnownPeers, TestProbe().ref, TestProbe().ref))
      val address: InetSocketAddress = new InetSocketAddress("0.0.0.0", 9000)
      val peerHandler: TestProbe = TestProbe()
      val connectedPeer: ConnectedPeer = ConnectedPeer(
        address,
        peerHandler.ref,
        Outgoing,
        Handshake(protocolToBytes(settingsWithKnownPeers.network.appVersion), "test node", Some(address), System.currentTimeMillis())
      )
      Thread.sleep(4000)
      peersKeeper ! BanPeer(connectedPeer, SentNetworkMessageWithTooManyModifiers)
      Thread.sleep(2000)
      peersKeeper.underlyingActor.blackList.contains(address.getAddress) shouldBe true
    }
  }
}