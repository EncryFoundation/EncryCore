package encry.network

import java.net.InetSocketAddress
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import encry.consensus.History.HistoryComparisonResult
import encry.network.DeliveryManager.GetStatusTrackerPeer
import encry.network.PeerConnectionHandler.{ConnectedPeer, Incoming}
import encry.network.SyncTracker.PeerPriorityStatus.PeerPriorityStatus
import org.scalatest.{BeforeAndAfterAll, FlatSpecLike, Matchers}
import scala.concurrent.duration._
import akka.pattern.ask
import akka.util.Timeout
import com.typesafe.scalalogging.StrictLogging
import encry.modifiers.InstanceFactory
import encry.modifiers.history.Block
import encry.network.NetworkController.ReceivableMessages.DataFromPeer
import encry.network.NodeViewSynchronizer.ReceivableMessages.HandshakedPeer
import encry.network.message.ModifiersSpec
import encry.settings.EncryAppSettings
import encry.utils.CoreTaggedTypes.{ModifierId, ModifierTypeId}
import encry.utils.{CoreTaggedTypes, EncryGenerator}
import encry.view.EncryNodeViewHolder.DownloadRequest
import org.scalatest.concurrent.ScalaFutures
import supertagged.@@
import scala.concurrent.Await

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

  implicit lazy val timeout: Timeout = Timeout(1.minutes)
  val settings: EncryAppSettings = EncryAppSettings.read
  val dm: ActorRef = system
    .actorOf(Props(classOf[DeliveryManager], None, TestProbe().ref, TestProbe().ref, system, settings))

  "Low priority test" should "shows LowPriority ( 3 )" in {

    val blocksV: Vector[Block] = (0 until 10).foldLeft(generateDummyHistory(settings), Vector.empty[Block]) {
      case ((prevHistory, blocks), _) =>
        val block: Block = generateNextBlock(prevHistory)
        (prevHistory.append(block.header).get._1.append(block.payload).get._1.reportModifierIsValid(block),
          blocks :+ block)
    }._2

    val newPeer = new InetSocketAddress("172.16.12.10", 9001)

    val cP: ConnectedPeer =
      ConnectedPeer(newPeer, dm, Incoming,
        Handshake(Version(1.toByte, 2.toByte, 3.toByte),
          "peer", Some(newPeer), System.currentTimeMillis())
      )

    dm ! HandshakedPeer(cP)

    val coll: Map[ModifierId, Array[Byte]] = blocksV.take(5).map(b => b.header).map { h => h.id -> h.bytes }.toMap

    val message: (Byte @@ CoreTaggedTypes.ModifierTypeId.Tag, Map[ModifierId, Array[Byte]]) = ModifierTypeId @@ (101: Byte) -> coll

    blocksV.foldLeft(Option.empty[ModifierId]) { case (prevModId, currBlock) =>
      dm ! DownloadRequest(ModifierTypeId @@ (101: Byte), currBlock.header.id, prevModId)
      Some(currBlock.header.id)
    }

    dm ! DataFromPeer(ModifiersSpec, message, cP)

    Thread.sleep(10000)

    val result = Await.result(
      (dm ? GetStatusTrackerPeer).mapTo[Map[ConnectedPeer, (HistoryComparisonResult, PeerPriorityStatus)]],
      1.minutes
    )

    result.get(cP).map(_._2).get shouldEqual 3
  }
}
