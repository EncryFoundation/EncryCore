package encry.network.PriorityTests

import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import akka.util.Timeout
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.History.HistoryComparisonResult
import encry.modifiers.InstanceFactory
import encry.modifiers.history.Block
import encry.network.BasicMessagesRepo.{Handshake, ModifiersNetworkMessage}
import encry.network.DeliveryManager.GetStatusTrackerPeer
import encry.network.NetworkController.ReceivableMessages.DataFromPeer
import encry.network.NodeViewSynchronizer.ReceivableMessages.HandshakedPeer
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

/**
  * This test simulates DeliveryManager behaviour connected with updating nodes priority while blockChain is not synced.
  *
  * Send handshake to the Delivery Manager from peer1.
  * Send downloadRequest for N modifiers to the Delivery manager.
  * Delivery manager must send requestModifier message for N modifiers to peer1.
  * Send N / 2 valid requested modifiers to the Delivery manager from peer1.
  * Check on Delivery manager that peer1 priority is LowPriority(3).
  */

class LowPriorityTest extends TestKit(ActorSystem("MySpecN"))
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
    .actorOf(Props(classOf[DeliveryManager], None, TestProbe().ref, TestProbe().ref, system, settings))

  "Low priority test" should "show LowPriority ( 3 )" in {

    val blocksV: Vector[Block] = (0 until 10).foldLeft(generateDummyHistory(settings), Vector.empty[Block]) {
      case ((prevHistory, blocks), _) =>
        val block: Block = generateNextBlock(prevHistory)
        (prevHistory.append(block.header).get._1.append(block.payload).get._1.reportModifierIsValid(block),
          blocks :+ block)
    }._2

    val newPeer = new InetSocketAddress("172.16.12.10", 9001)

    val cP: ConnectedPeer =
      ConnectedPeer(newPeer, dm, Incoming,
        Handshake(protocolToBytes(settings.network.appVersion), "peer3", Some(newPeer), System.currentTimeMillis()))

    dm ! HandshakedPeer(cP)

    val coll: Map[ModifierId, Array[Byte]] = blocksV.take(5).map(b => b.header).map { h => h.id -> h.bytes }.toMap

    val message: (Byte @@ CoreTaggedTypes.ModifierTypeId.Tag, Map[ModifierId, Array[Byte]]) = ModifierTypeId @@ (101: Byte) -> coll

    blocksV.foldLeft(Option.empty[ModifierId]) { case (prevModId, currBlock) =>
      dm ! DownloadRequest(ModifierTypeId @@ (101: Byte), currBlock.header.id, prevModId)
      Some(currBlock.header.id)
    }

    dm ! DataFromPeer(ModifiersNetworkMessage(message), cP)

    Thread.sleep(10000)

    val result = Await.result(
      (dm ? GetStatusTrackerPeer).mapTo[Map[ConnectedPeer, (HistoryComparisonResult, PeerPriorityStatus)]],
      1.minutes
    )

    result.get(cP).map(_._2).get shouldEqual 3
  }
}
