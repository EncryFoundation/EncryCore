//package encry.network.PriorityTests
//
//import java.net.InetSocketAddress
//import akka.actor.{ActorRef, ActorSystem, Props}
//import akka.pattern.ask
//import akka.testkit.{ImplicitSender, TestKit, TestProbe}
//import akka.util.Timeout
//import com.typesafe.scalalogging.StrictLogging
//import encry.consensus.History.HistoryComparisonResult
//import encry.modifiers.InstanceFactory
//import encry.modifiers.history.Block
//import encry.network.DeliveryManager.GetStatusTrackerPeer
//import encry.network.NetworkController.ReceivableMessages.DataFromPeer
//import encry.network.NodeViewSynchronizer.ReceivableMessages.HandshakedPeer
//import encry.network.PeerConnectionHandler.{ConnectedPeer, Incoming}
//import encry.network.SyncTracker.PeerPriorityStatus.PeerPriorityStatus
//import encry.network.message.ModifiersSpec
//import encry.network.{DeliveryManager, Handshake, Version}
//import encry.settings.EncryAppSettings
//import encry.utils.CoreTaggedTypes.{ModifierId, ModifierTypeId}
//import encry.utils.{CoreTaggedTypes, EncryGenerator}
//import encry.view.EncryNodeViewHolder.DownloadRequest
//import org.scalatest.concurrent.ScalaFutures
//import org.scalatest.{BeforeAndAfterAll, FlatSpecLike, Matchers}
//import supertagged.@@
//import scala.concurrent.Await
//import scala.concurrent.duration._
//
///**
//  * This test simulates DeliveryManager behaviour connected with the logic of choosing nodes relative to priority.
//  *
//  * First - send handshake message from peer1.
//  * Send downloadRequest for N modifiers to the Delivery manager.
//  * Delivery manager must send requestModifier message for N modifiers to peer1.
//  * Send N valid requested modifiers to the Delivery manager from peer1.
//  * Check on Delivery manager that peer1 priority is HighPriority(4).
//  *
//  * Second - send handshake message from peer2.
//  * Send downloadRequest for N modifiers to the Delivery manager.
//  * Delivery manager must send requestModifier message for M modifiers to peer1.
//  * Send M - K where ((M - K) / M) < LowPriority valid requested modifiers to the Delivery manager from peer1.
//  * Check on Delivery manager that peer1 priority is BadNode(1).
//  *
//  * Third - send downloadRequest for N modifiers to the delivery manager.
//  * Delivery manager must send requestModifier message for M modifiers to peer2.
//  * Send M valid requested modifiers to the Delivery manager from peer2.
//  * Check peer1 priority on this node - must be BadNode(1).
//  * Check peer2 priority on this node - must be HighPriority(4).
//  */
//
//class PrioritySeveralNodesCommunicationTest extends TestKit(ActorSystem("MySpecN"))
//  with ImplicitSender
//  with FlatSpecLike
//  with Matchers
//  with BeforeAndAfterAll
//  with ScalaFutures
//  with InstanceFactory
//  with EncryGenerator
//  with StrictLogging {
//
//  override def afterAll: Unit = TestKit.shutdownActorSystem(system)
//
//  implicit val timeout: Timeout = Timeout(1.minutes)
//  val settings: EncryAppSettings = EncryAppSettings.read
//  val dm: ActorRef = system
//    .actorOf(Props(classOf[DeliveryManager], None, TestProbe().ref, TestProbe().ref, system, settings))
//
//  "Several nodes test" should "show right behavior" in {
//
//    val blocksV: Vector[Block] = (0 until 20).foldLeft(generateDummyHistory(settings), Vector.empty[Block]) {
//      case ((prevHistory, blocks), _) =>
//        val block: Block = generateNextBlock(prevHistory)
//        (prevHistory.append(block.header).get._1.append(block.payload).get._1.reportModifierIsValid(block),
//          blocks :+ block)
//    }._2
//
//    val newPeer1 = new InetSocketAddress("172.16.12.10", 9001)
//    val newPeer2 = new InetSocketAddress("172.16.13.10", 9001)
//
//    val cP1: ConnectedPeer =
//      ConnectedPeer(newPeer1, dm, Incoming,
//        Handshake(Version(1.toByte, 2.toByte, 3.toByte),
//          "peer1", Some(newPeer1), System.currentTimeMillis())
//      )
//
//    val cP2: ConnectedPeer =
//      ConnectedPeer(newPeer2, dm, Incoming,
//        Handshake(Version(1.toByte, 2.toByte, 3.toByte),
//          "peer2", Some(newPeer2), System.currentTimeMillis())
//      )
//
//    dm ! HandshakedPeer(cP1)
//
//    blocksV.take(10).foldLeft(Option.empty[ModifierId]) { case (prevModId, currBlock) =>
//      dm ! DownloadRequest(ModifierTypeId @@ (101: Byte), currBlock.header.id, prevModId)
//      Some(currBlock.header.id)
//    }
//
//    val coll1: Map[ModifierId, Array[Byte]] = blocksV.take(10).map(b => b.header).map { h => h.id -> h.bytes }.toMap
//
//    val message1: (Byte @@ CoreTaggedTypes.ModifierTypeId.Tag, Map[ModifierId, Array[Byte]]) =
//      ModifierTypeId @@ (101: Byte) -> coll1
//
//    dm ! DataFromPeer(ModifiersSpec, message1, cP1)
//
//    Thread.sleep(10000)
//
//    val result1 = Await.result(
//      (dm ? GetStatusTrackerPeer).mapTo[Map[ConnectedPeer, (HistoryComparisonResult, PeerPriorityStatus)]],
//      1.minutes
//    )
//
//    result1.get(cP1).map(_._2).get shouldEqual 4
//
//    dm ! HandshakedPeer(cP2)
//
//    blocksV.takeRight(10).foldLeft(Option.empty[ModifierId]) { case (prevModId, currBlock) =>
//      dm ! DownloadRequest(ModifierTypeId @@ (101: Byte), currBlock.header.id, prevModId)
//      Some(currBlock.header.id)
//    }
//
//    Thread.sleep(10000)
//
//    val result2 = Await.result(
//      (dm ? GetStatusTrackerPeer).mapTo[Map[ConnectedPeer, (HistoryComparisonResult, PeerPriorityStatus)]],
//      1.minutes
//    )
//
//    result2.get(cP1).map(_._2).get shouldEqual 1
//
//    blocksV.takeRight(10).foldLeft(Option.empty[ModifierId]) { case (prevModId, currBlock) =>
//      dm ! DownloadRequest(ModifierTypeId @@ (101: Byte), currBlock.header.id, prevModId)
//      Some(currBlock.header.id)
//    }
//
//    val coll2: Map[ModifierId, Array[Byte]] = blocksV.takeRight(10).map(b => b.header).map { h => h.id -> h.bytes }.toMap
//
//    val message2: (Byte @@ CoreTaggedTypes.ModifierTypeId.Tag, Map[ModifierId, Array[Byte]]) =
//      ModifierTypeId @@ (101: Byte) -> coll2
//
//    dm ! DataFromPeer(ModifiersSpec, message2, cP2)
//
//    Thread.sleep(15000)
//
//    val result3 = Await.result(
//      (dm ? GetStatusTrackerPeer).mapTo[Map[ConnectedPeer, (HistoryComparisonResult, PeerPriorityStatus)]],
//      1.minutes
//    )
//
//    result3.get(cP1).map(_._2).get shouldEqual 1
//    result3.get(cP2).map(_._2).get shouldEqual 4
//  }
//}
