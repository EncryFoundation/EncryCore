package encry.network.PriorityTests

import java.net.InetSocketAddress
import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.ask
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import akka.util.Timeout
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.History.HistoryComparisonResult
import encry.modifiers.InstanceFactory
import encry.modifiers.history.Block
import encry.network.BasicMessagesRepo.{Handshake, ModifiersNetworkMessage}
import encry.network.DeliveryManager.{FullBlockChainIsSynced, GetSyncTrackerPeer}
import encry.network.NetworkController.ReceivableMessages.DataFromPeer
import encry.network.NodeViewSynchronizer.ReceivableMessages.{HandshakedPeer, UpdatedHistory}
import encry.network.PeerConnectionHandler.{ConnectedPeer, Incoming}
import encry.network.SyncTracker.PeerPriorityStatus.PeerPriorityStatus
import encry.network.DeliveryManager
import encry.settings.EncryAppSettings
import encry.utils.CoreTaggedTypes.{ModifierId, ModifierTypeId}
import encry.utils.{CoreTaggedTypes, EncryGenerator}
import encry.view.EncryNodeViewHolder.DownloadRequest
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{BeforeAndAfterAll, FlatSpecLike, Matchers}
import supertagged.@@
import scala.concurrent.Await
import scala.concurrent.duration._
import encry.view.history.EncryHistory

/**
  * This test simulates DeliveryManager behaviour connected with updating nodes priority while blockChain synced.
  *
  * Send handshake to the Delivery Manager from peer1, peer2, peer3.
  * Send downloadRequest for N modifiers to the Delivery manager.
  * Delivery manager must send requestModifier message for N modifiers to all peers.
  * Send N valid requested modifiers to the Delivery manager from peer1.
  * Send N / 2 valid requested modifiers to the Delivery manager from peer2.
  * Send 0 valid requested modifiers to the Delivery manager from peer3.
  * Check on Delivery manager that peer1 priorities is HighPriority(4).
  * Check on Delivery manager that peer2 priorities is LowPriority(3).
  * Check on Delivery manager that peer3 priorities is BadNode(1).
  */

class DifferentPrioritiesSyncedChainTest extends TestKit(ActorSystem("MySpecN"))
  with ImplicitSender
  with FlatSpecLike
  with Matchers
  with BeforeAndAfterAll
  with ScalaFutures
  with InstanceFactory
  with EncryGenerator
  with StrictLogging {

  override def afterAll: Unit = TestKit.shutdownActorSystem(system)

  implicit val timeout: Timeout = Timeout(1.minutes)
  val settings: EncryAppSettings = EncryAppSettings.read
  val dm: ActorRef = system
    .actorOf(DeliveryManager.props(None, TestProbe().ref, TestProbe().ref, settings))

  "Priority synced chain test" should "show shows right behavior" in {

    val history: EncryHistory = generateDummyHistory(settings)
    dm ! UpdatedHistory(history)

    dm ! FullBlockChainIsSynced

    val blocksV: Vector[Block] = (0 until 10).foldLeft(generateDummyHistory(settings), Vector.empty[Block]) {
      case ((prevHistory, blocks), _) =>
        val block: Block = generateNextBlock(prevHistory)
        (prevHistory.append(block.header).get._1.append(block.payload).get._1.reportModifierIsValid(block),
          blocks :+ block)
    }._2

    val newPeer1 = new InetSocketAddress("172.16.12.10", 9001)
    val newPeer2 = new InetSocketAddress("172.16.13.10", 9001)
    val newPeer3 = new InetSocketAddress("172.16.14.10", 9001)

    val cP1: ConnectedPeer =
      ConnectedPeer(newPeer1, dm, Incoming,
        Handshake(protocolToBytes(settings.network.appVersion), "peer1", Some(newPeer1), System.currentTimeMillis()))

    val cP2: ConnectedPeer =
      ConnectedPeer(newPeer2, dm, Incoming,
        Handshake(protocolToBytes(settings.network.appVersion), "pee2r", Some(newPeer2), System.currentTimeMillis()))

    val cP3: ConnectedPeer =
      ConnectedPeer(newPeer3, dm, Incoming,
        Handshake(protocolToBytes(settings.network.appVersion), "peer3", Some(newPeer3), System.currentTimeMillis()))

    dm ! HandshakedPeer(cP1)
    dm ! HandshakedPeer(cP2)
    dm ! HandshakedPeer(cP3)

    blocksV.foreach(block => dm ! DownloadRequest(ModifierTypeId @@ (101: Byte), block.header.id, None))

    val coll1: Map[ModifierId, Array[Byte]] = blocksV.map(b => b.header).map { h => h.id -> h.bytes }.toMap

    val message1: (Byte @@ CoreTaggedTypes.ModifierTypeId.Tag, Map[ModifierId, Array[Byte]]) =
      ModifierTypeId @@ (101: Byte) -> coll1

    val coll2: Map[ModifierId, Array[Byte]] = blocksV.take(5).map(b => b.header).map { h => h.id -> h.bytes }.toMap

    val message2: (Byte @@ CoreTaggedTypes.ModifierTypeId.Tag, Map[ModifierId, Array[Byte]]) =
      ModifierTypeId @@ (101: Byte) -> coll2

    dm ! DataFromPeer(ModifiersNetworkMessage(message1), cP1)
    dm ! DataFromPeer(ModifiersNetworkMessage(message2), cP2)

    Thread.sleep(10000)

    val result = Await.result(
      (dm ? GetSyncTrackerPeer).mapTo[Map[ConnectedPeer, (HistoryComparisonResult, PeerPriorityStatus)]],
      1.minutes
    )

    result.get(cP1).map(_._2).get shouldEqual 4
    result.get(cP2).map(_._2).get shouldEqual 3
    result.get(cP3).map(_._2).get shouldEqual 1
  }
}