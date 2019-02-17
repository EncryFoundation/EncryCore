package encry.network

import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import encry.consensus.History.HistoryComparisonResult
import encry.network.DeliveryManager.GetStatusTrackerPeer
import encry.network.PeerConnectionHandler.{ConnectedPeer, Incoming}
import encry.network.SyncTracker.PeerPriorityStatus.PeerPriorityStatus
import org.scalatest.{BeforeAndAfterAll, FlatSpecLike, Matchers, WordSpecLike}

import scala.concurrent.duration._
import akka.pattern.ask
import akka.util.Timeout
import encry.modifiers.InstanceFactory
import encry.modifiers.history.{Block, Header}
import encry.network.NetworkController.ReceivableMessages.DataFromPeer
import encry.network.message.ModifiersSpec
import encry.settings.EncryAppSettings
import encry.utils.CoreTaggedTypes.{ModifierId, ModifierTypeId}
import encry.utils.{CoreTaggedTypes, EncryGenerator}
import encry.view.EncryNodeViewHolder.DownloadRequest
import encry.view.history.EncryHistory
import org.scalatest.concurrent.ScalaFutures
import supertagged.@@

import scala.concurrent.Future

class PriorityTest extends TestKit(ActorSystem("MySpec")) with ImplicitSender
  with FlatSpecLike with Matchers with BeforeAndAfterAll with ScalaFutures with InstanceFactory with EncryGenerator {

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  implicit lazy val timeout: Timeout = Timeout(1.minutes)

  val settings: EncryAppSettings = EncryAppSettings.read
  val dm: ActorRef = system.actorOf(Props(classOf[DeliveryManager], None))

  "BlockListener" should "process valid chain switching msg" in {

    val blocks10Chain1: Vector[Block] = (0 until 10).foldLeft(generateDummyHistory(settings), Vector.empty[Block]) {
      case ((prevHistory, blocks), _) =>
        val block: Block = generateNextBlock(prevHistory)
        (prevHistory.append(block.header).get._1.append(block.payload).get._1.reportModifierIsValid(block),
          blocks :+ block)
    }._2

    val blocks10Chain2: Vector[Block] = (0 until 10).foldLeft(generateDummyHistory(settings), Vector.empty[Block]) {
      case ((prevHistory, blocks), _) =>
        val block: Block = generateNextBlock(prevHistory)
        (prevHistory.append(block.header).get._1.append(block.payload).get._1.reportModifierIsValid(block),
          blocks :+ block)
    }._2

    val blocks10Chain3: Vector[Block] = (0 until 10).foldLeft(generateDummyHistory(settings), Vector.empty[Block]) {
      case ((prevHistory, blocks), _) =>
        val block: Block = generateNextBlock(prevHistory)
        (prevHistory.append(block.header).get._1.append(block.payload).get._1.reportModifierIsValid(block),
          blocks :+ block)
    }._2

    val newPeer1 = new InetSocketAddress("172.16.11.10", 9001)
    val newPeer2 = new InetSocketAddress("172.16.12.10", 9001)
    val newPeer3 = new InetSocketAddress("172.16.13.10", 9001)

    val coll: Map[ModifierId, Array[Byte]] = blocks10Chain1.map(b => b.header).map { h =>
      h.id -> h.bytes
    }.toMap

    val message: (Byte @@ CoreTaggedTypes.ModifierTypeId.Tag, Map[ModifierId, Array[Byte]]) = ModifierTypeId @@ (101: Byte) -> coll

    val c1: ConnectedPeer =
      ConnectedPeer(newPeer1, dm, Incoming,
        Handshake(Version(1.toByte, 2.toByte, 3.toByte),
          "peer1", Some(newPeer1), System.currentTimeMillis())
      )

    blocks10Chain1.foldLeft(Option.empty[ModifierId]) { case (prevModId, currBlock) =>
      dm ! DownloadRequest(ModifierTypeId @@ (101: Byte), currBlock.header.id, prevModId)
      Some(currBlock.header.id)
    }

    dm ! DataFromPeer(ModifiersSpec, message, c1)

    val coll2: Map[ModifierId, Array[Byte]] = blocks10Chain2.take(5).map(b => b.header).map { h =>
      h.id -> h.bytes
    }.toMap

    val messag2: (Byte @@ CoreTaggedTypes.ModifierTypeId.Tag, Map[ModifierId, Array[Byte]]) = ModifierTypeId @@ (101: Byte) -> coll2

    val c2: ConnectedPeer =
      ConnectedPeer(newPeer2, dm, Incoming,
        Handshake(Version(1.toByte, 2.toByte, 3.toByte),
          "peer2", Some(newPeer2), System.currentTimeMillis())
      )

    blocks10Chain2.foldLeft(Option.empty[ModifierId]) { case (prevModId, currBlock) =>
      dm ! DownloadRequest(ModifierTypeId @@ (101: Byte), currBlock.header.id, prevModId)
      Some(currBlock.header.id)
    }

    dm ! DataFromPeer(ModifiersSpec, messag2, c2)

    val coll3: Map[ModifierId, Array[Byte]] = blocks10Chain3.map(b => b.header).map { h =>
      h.id -> h.bytes
    }.toMap

    val messag3: (Byte @@ CoreTaggedTypes.ModifierTypeId.Tag, Map[ModifierId, Array[Byte]]) = ModifierTypeId @@ (101: Byte) -> Map.empty

    val c3: ConnectedPeer =
      ConnectedPeer(newPeer3, dm, Incoming,
        Handshake(Version(1.toByte, 2.toByte, 3.toByte),
          "peer3", Some(newPeer3), System.currentTimeMillis())
      )

    blocks10Chain3.foldLeft(Option.empty[ModifierId]) { case (prevModId, currBlock) =>
      dm ! DownloadRequest(ModifierTypeId @@ (101: Byte), currBlock.header.id, prevModId)
      Some(currBlock.header.id)
    }

    val c: Map[ConnectedPeer, (HistoryComparisonResult, PeerPriorityStatus)] = (dm ? GetStatusTrackerPeer)
      .mapTo[Map[ConnectedPeer, (HistoryComparisonResult, PeerPriorityStatus)]].futureValue

    val a1 = c.get(c1)
    a1.map(_._2) shouldEqual 4
    val a2 = c.get(c2)
    a2.map(_._2) shouldEqual 3
    val a3 = c.get(c3)
    a3.map(_._2) shouldEqual 1
  }
}