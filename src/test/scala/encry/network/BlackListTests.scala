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
import encry.network.PeersKeeper.{BanPeer, UpdatedPeersCollection}
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

  val settings: EncryAppSettings = DummyEncryAppSettingsReader.read
  implicit val system: ActorSystem = ActorSystem()

  override def afterAll(): Unit = system.terminate()

  /*
    Unit tests
   */
  "Black list" should {
    "temporary ban requested peer correctly" in {
      val blackList: BlackList = new BlackList(settings)
      val peer: InetAddress = new InetSocketAddress("0.0.0.0", 9000).getAddress
      blackList.banPeer(SemanticallyInvalidModifier, peer)
      blackList.contains(peer) shouldBe true
    }
    "clean black list from peers with expired ban time which were banned by temporary ban" in {
      val blackList: BlackList = new BlackList(settings)
      val peer: InetAddress = new InetSocketAddress("0.0.0.0", 9000).getAddress
      blackList.banPeer(SyntacticallyInvalidModifier, peer)
      Thread.sleep(2000)
      blackList.cleanupBlackList()
      blackList.contains(peer) shouldBe false
    }
    "don't remove peer from black list before ban time expired" in {
      val blackList: BlackList = new BlackList(settings)
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
      val peersKeeper: TestActorRef[PeersKeeper] = TestActorRef[PeersKeeper](PeersKeeper.props(settings, TestProbe().ref))
      val address: InetSocketAddress = new InetSocketAddress("0.0.0.0", 9000)
      val peerHandler: TestProbe = TestProbe()
      val connectedPeer: ConnectedPeer = ConnectedPeer(
        address,
        peerHandler.ref,
        Outgoing,
        Handshake(protocolToBytes(settings.network.appVersion), "test node", Some(address), System.currentTimeMillis())
      )
      peersKeeper ! BanPeer(connectedPeer, SpamSender)
      peerHandler.expectMsg(CloseConnection)
      peersKeeper.underlyingActor.blackList.contains(address.getAddress) shouldBe true
    }
    "cleanup black list by scheduler correctly" in {
      val peersKeeper: TestActorRef[PeersKeeper] = TestActorRef[PeersKeeper](PeersKeeper.props(settings, TestProbe().ref))
      val address: InetSocketAddress = new InetSocketAddress("0.0.0.0", 9000)
      val peerHandler: TestProbe = TestProbe()
      val connectedPeer: ConnectedPeer = ConnectedPeer(
        address,
        peerHandler.ref,
        Outgoing,
        Handshake(protocolToBytes(settings.network.appVersion), "test node", Some(address), System.currentTimeMillis())
      )
      peersKeeper ! BanPeer(connectedPeer, SentPeersMessageWithoutRequest)
      Thread.sleep(6000)
      peersKeeper.underlyingActor.blackList.contains(address.getAddress) shouldBe false
    }
    "don't remove peer from black list before ban time expired" in {
      val peersKeeper: TestActorRef[PeersKeeper] = TestActorRef[PeersKeeper](PeersKeeper.props(settings, TestProbe().ref))
      val address: InetSocketAddress = new InetSocketAddress("0.0.0.0", 9000)
      val peerHandler: TestProbe = TestProbe()
      val connectedPeer: ConnectedPeer = ConnectedPeer(
        address,
        peerHandler.ref,
        Outgoing,
        Handshake(protocolToBytes(settings.network.appVersion), "test node", Some(address), System.currentTimeMillis())
      )
      Thread.sleep(4000)
      peersKeeper ! BanPeer(connectedPeer, SentNetworkMessageWithTooManyModifiers)
      Thread.sleep(2000)
      peersKeeper.underlyingActor.blackList.contains(address.getAddress) shouldBe true
    }
  }

  "DeliveryManager" should {
    "ban peer which sent invalid modifier" in {
      val address: InetSocketAddress = new InetSocketAddress("0.0.0.0", 9000)
      val peerHandler: TestProbe = TestProbe()
      val connectedPeer: ConnectedPeer = ConnectedPeer(
        address,
        peerHandler.ref,
        Outgoing,
        Handshake(protocolToBytes(settings.network.appVersion), "test node", Some(address), System.currentTimeMillis())
      )
      val history: EncryHistory = generateDummyHistory(settings)
      val timestamp1 = System.currentTimeMillis()
      Thread.sleep(1000)
      val timestamp2 = System.currentTimeMillis()
      val header_first: Header = Header(
        1.toByte,
        ModifierId @@ Random.randomBytes(),
        Digest32 @@ Random.randomBytes(32),
        ADDigest @@ Random.randomBytes(33),
        Digest32 @@ Random.randomBytes(),
        timestamp2,
        2,
        scala.util.Random.nextLong(),
        TestNetConstants.InitialDifficulty,
        EquihashSolution(Seq(1, 3))
      )
      val header_second: Header = Header(
        1.toByte,
        header_first.id,
        Digest32 @@ Random.randomBytes(32),
        ADDigest @@ Random.randomBytes(33),
        Digest32 @@ Random.randomBytes(),
        timestamp1,
        1,
        scala.util.Random.nextLong(),
        TestNetConstants.InitialDifficulty,
        EquihashSolution(Seq(1, 3))
      )
      val history1: EncryHistory = history.append(header_first).get._1
      val nodeViewSync: TestProbe = TestProbe()
      val deliveryManager: TestActorRef[DeliveryManager] =
        TestActorRef[DeliveryManager](DeliveryManager
          .props(None, TestProbe().ref, TestProbe().ref, settings, TestProbe().ref, nodeViewSync.ref))
      deliveryManager ! UpdatedHistory(history1)
      deliveryManager ! StartMining
      deliveryManager ! FullBlockChainIsSynced
      val updatedPeersCollection: Map[InetAddress, (ConnectedPeer, History.Older.type, InitialPriority)] =
        Map(connectedPeer.socketAddress.getAddress -> (connectedPeer, Older, InitialPriority()))

      deliveryManager ! UpdatedPeersCollection(updatedPeersCollection)

      deliveryManager ! RequestFromLocal(connectedPeer, Header.modifierTypeId, Seq(header_second.id))
      deliveryManager ! DataFromPeer(
        ModifiersNetworkMessage(
          Header.modifierTypeId, Map(header_second.id -> HeaderProtoSerializer.toProto(header_second).toByteArray)),
        connectedPeer
      )
      nodeViewSync.expectMsg(BanPeer(connectedPeer, SemanticallyInvalidModifier))
      deliveryManager.underlyingActor.receivedModifiers.contains(DMUtils.toKey(header_second.id)) shouldBe false
    }
  }
}