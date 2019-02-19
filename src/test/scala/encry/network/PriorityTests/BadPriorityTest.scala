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
import encry.network.DeliveryManager.GetStatusTrackerPeer
import encry.network.NodeViewSynchronizer.ReceivableMessages.HandshakedPeer
import encry.network.PeerConnectionHandler.{ConnectedPeer, Incoming}
import encry.network.SyncTracker.PeerPriorityStatus.PeerPriorityStatus
import encry.network.{DeliveryManager, Handshake, Version}
import encry.settings.EncryAppSettings
import encry.utils.CoreTaggedTypes.{ModifierId, ModifierTypeId}
import encry.utils.EncryGenerator
import encry.view.EncryNodeViewHolder.DownloadRequest
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{BeforeAndAfterAll, FlatSpecLike, Matchers}
import scala.concurrent.Await
import scala.concurrent.duration._

/**
  * This test simulates DeliveryManager behaviour connected with updating nodes priority while blockChain is not synced.
  *
  * Send handshake to the Delivery Manager from peer1.
  * Send downloadRequest for N modifiers to the Delivery manager.
  * Delivery manager must send requestModifier message for N modifiers to peer1.
  * Do not send any modifiers to the Delivery manager from peer1.
  * Check on Delivery manager that peer1 priority is BadNode(1).
  */

class BadPriorityTest extends TestKit(ActorSystem("MySpecN"))
  with ImplicitSender
  with FlatSpecLike
  with Matchers
  with BeforeAndAfterAll
  with ScalaFutures
  with InstanceFactory
  with EncryGenerator
  with StrictLogging {

  override def afterAll: Unit = TestKit.shutdownActorSystem(system)

  implicit lazy val timeout: Timeout = Timeout(1.minutes)
  val settings: EncryAppSettings = EncryAppSettings.read
  val dm: ActorRef = system
    .actorOf(Props(classOf[DeliveryManager], None, TestProbe().ref, TestProbe().ref, system, settings))

  "Bad priority test" should "show BadPriority ( 1 )" in {

    val blocks1V: Vector[Block] = (0 until 10).foldLeft(generateDummyHistory(settings), Vector.empty[Block]) {
      case ((prevHistory, blocks), _) =>
        val block: Block = generateNextBlock(prevHistory)
        (prevHistory.append(block.header).get._1.append(block.payload).get._1.reportModifierIsValid(block),
          blocks :+ block)
    }._2

    val newPeer = new InetSocketAddress("172.16.13.10", 9001)

    val cP: ConnectedPeer =
      ConnectedPeer(newPeer, dm, Incoming,
        Handshake(Version(1.toByte, 2.toByte, 3.toByte),
          "peer", Some(newPeer), System.currentTimeMillis())
      )

    dm ! HandshakedPeer(cP)

    blocks1V.foldLeft(Option.empty[ModifierId]) { case (prevModId, currBlock) =>
      dm ! DownloadRequest(ModifierTypeId @@ (101: Byte), currBlock.header.id, prevModId)
      Some(currBlock.header.id)
    }

    Thread.sleep(10000)

    val result = Await.result(
      (dm ? GetStatusTrackerPeer).mapTo[Map[ConnectedPeer, (HistoryComparisonResult, PeerPriorityStatus)]],
      1.minutes
    )

    result.get(cP).map(_._2).get shouldEqual 1
  }
}
