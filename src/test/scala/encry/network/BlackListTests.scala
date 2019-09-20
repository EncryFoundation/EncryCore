package encry.network

import java.io.File
import java.net.{InetAddress, InetSocketAddress}

import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestProbe}
import encry.modifiers.InstanceFactory
import encry.network.BlackList.BanReason._
import encry.network.PeerConnectionHandler.{ConnectedPeer, Outgoing}
import encry.network.PeerConnectionHandler.ReceivableMessages.CloseConnection
import encry.network.PeersKeeper.BanPeer
import encry.settings.TestNetSettings
import encry.utils.{FileHelper, NetworkTimeProvider, TestHelper}
import encry.view.wallet.EncryWallet
import org.encryfoundation.common.network.BasicMessagesRepo.Handshake
import org.scalatest.{BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike}
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import akka.util.Timeout
import encry.EncryApp.settings
import encry.modifiers.InstanceFactory
import encry.network.DeliveryManager.FullBlockChainIsSynced
import encry.network.ModifiersToNetworkUtils
import encry.network.NodeViewSynchronizer.ReceivableMessages.{SemanticallySuccessfulModifier, UpdatedHistory}
import encry.settings.{EncryAppSettings, Settings}
import encry.storage.VersionalStorage
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.storage.iodb.versionalIODB.IODBWrapper
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
import encry.utils.{FileHelper, TestHelper}
import encry.view.{ModifiersCache, actors}
import encry.view.actors.{HistoryApplicator, StateApplicator}
import encry.view.actors.HistoryApplicator.{ModifierToHistoryAppending, StartModifiersApplicationOnStateApplicator}
import encry.view.actors.NodeViewHolder.ReceivableMessages.{LocallyGeneratedBlock, ModifierFromRemote}
import encry.view.actors.StateApplicator.NotificationAboutSuccessfullyAppliedModifier
import encry.view.history.History
import encry.view.state.{BoxHolder, UtxoState}
import encry.view.wallet.EncryWallet
import io.iohk.iodb.LSMStore
import org.encryfoundation.common.modifiers.history.{Block, Header, HeaderProtoSerializer, Payload, PayloadProtoSerializer}
import org.encryfoundation.common.utils.TaggedTypes.{Height, ModifierId}
import org.encryfoundation.common.utils.constants.TestNetConstants
import org.iq80.leveldb.Options
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import akka.testkit.{TestActorRef, TestKit}
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.utils.Algos

import scala.concurrent.duration._

class BlackListTests extends WordSpecLike
  with Matchers
  with BeforeAndAfterAll
  with InstanceFactory
  with OneInstancePerTest
  with TestNetSettings {

  implicit val system: ActorSystem = ActorSystem()

  override def afterAll(): Unit = system.terminate()

  val knowPeersSettings = testNetSettings.copy(
    network = settings.network.copy(
      knownPeers = Seq(new InetSocketAddress("172.16.11.11", 9001)),
      connectOnlyWithKnownPeers = Some(true)
    ),
    blackList = settings.blackList.copy(
      banTime = 2 seconds,
      cleanupTime = 3 seconds
    ))

  def utxoFromBoxHolder(boxHolder: BoxHolder, dir: File, settings: EncryAppSettings): UtxoState = {
    val storage = settings.storage.state match {
      case VersionalStorage.IODB =>
        IODBWrapper(new LSMStore(dir, keepVersions = TestNetConstants.DefaultKeepVersions))
      case VersionalStorage.LevelDB =>
        val levelDBInit = LevelDbFactory.factory.open(dir, new Options)
        VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB, keySize = 32))
    }
    storage.insert(
      StorageVersion @@ Array.fill(32)(0: Byte),
      boxHolder.boxes.values.map(bx => (StorageKey !@@ bx.id, StorageValue @@ bx.bytes)).toList
    )
    new UtxoState(storage, settings.constants, Height @@ 0, 0)
  }

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