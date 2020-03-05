package encry.network

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import akka.testkit.{ TestActorRef, TestProbe }
import encry.modifiers.InstanceFactory
import encry.network.BlackList.BanReason._
import encry.network.DownloadedModifiersValidator.{ InvalidModifier, ModifiersForValidating }
import encry.network.NodeViewSynchronizer.ReceivableMessages.{ ChangedHistory, UpdatedHistory }
import encry.network.PeerConnectionHandler.{ ConnectedPeer, Outgoing }
import encry.network.PeersKeeper.BanPeer
import encry.settings.TestNetSettings
//import encry.view.NodeViewHolder.ReceivableMessages.ModifierFromRemote
import encry.view.history.History
import org.encryfoundation.common.crypto.equihash.EquihashSolution
import org.encryfoundation.common.modifiers.history.{
  Block,
  Header,
  HeaderProtoSerializer,
  Payload,
  PayloadProtoSerializer
}
import org.encryfoundation.common.network.BasicMessagesRepo.Handshake
import org.encryfoundation.common.utils.TaggedTypes.{ Height, ModifierId }
import org.scalatest.{ BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike }
import scorex.crypto.hash.Digest32
import scorex.utils.Random

//class DownloadedModifiersValidatorTests
//    extends WordSpecLike
//    with Matchers
//    with BeforeAndAfterAll
//    with InstanceFactory
//    with OneInstancePerTest
//    with TestNetSettings {
//
//  implicit val system: ActorSystem = ActorSystem()
//
//  override def afterAll(): Unit = system.terminate()
//
//  "DownloadedModifiersValidatorTests" should {
//    "find too old header by height" in {
//      val nodeViewHolder  = TestProbe()
//      val peersKeeper     = TestProbe()
//      val nodeViewSync    = TestProbe()
//      val mempool         = TestProbe()
//
//      val downloadedModifiersValidator = TestActorRef[DownloadedModifiersValidator](
//        DownloadedModifiersValidator.props(testNetSettings.constants.ModifierIdSize,
//                                           nodeViewHolder.ref,
//                                           peersKeeper.ref,
//                                           nodeViewSync.ref,
//                                           mempool.ref,
//                                           None,
//                                           settings)
//      )
//      val address: InetSocketAddress = new InetSocketAddress("0.0.0.0", 9000)
//      val peerHandler: TestProbe     = TestProbe()
//      val connectedPeer: ConnectedPeer = ConnectedPeer(
//        address,
//        peerHandler.ref,
//        Outgoing,
//        Handshake(protocolToBytes(testNetSettings.network.appVersion),
//                  "test node",
//                  Some(address),
//                  System.currentTimeMillis())
//      )
//
//      val (history, _) = generateBlocks(200, generateDummyHistory(testNetSettings))
//      downloadedModifiersValidator ! UpdatedHistory(history)
//      val invalidHeader = generateGenesisBlock(Height @@ 1)
//
//      val mods: Map[ModifierId, Array[Byte]] = List(invalidHeader)
//        .map(
//          b => b.header.id -> HeaderProtoSerializer.toProto(b.header).toByteArray
//        )
//        .toMap
//      import scala.concurrent.duration._
//      downloadedModifiersValidator ! ModifiersForValidating(connectedPeer, Header.modifierTypeId, mods)
//      peersKeeper.expectMsgPF(10.seconds) {
//        case BanPeer(connected, _) => connected == connectedPeer
//      }
//      val validHeightHeader = generateGenesisBlock(Height @@ 200)
//
//      val mods1: Map[ModifierId, Array[Byte]] = List(validHeightHeader)
//        .map(
//          b => b.header.id -> HeaderProtoSerializer.toProto(b.header).toByteArray
//        )
//        .toMap
//      downloadedModifiersValidator ! ModifiersForValidating(connectedPeer, Header.modifierTypeId, mods1)
//      nodeViewHolder.expectMsgPF(10.seconds) {
//        case ModifierFromRemote(mod) => mod == validHeightHeader.header
//      }
//    }
//    "find corrupted header" in {
//      val nodeViewHolder  = TestProbe()
//      val peersKeeper     = TestProbe()
//      val deliveryManager = TestProbe()
//      val nodeViewSync    = TestProbe()
//      val mempool         = TestProbe()
//
//      val downloadedModifiersValidator = TestActorRef[DownloadedModifiersValidator](
//        DownloadedModifiersValidator.props(testNetSettings.constants.ModifierIdSize,
//                                           nodeViewHolder.ref,
//                                           peersKeeper.ref,
//                                           nodeViewSync.ref,
//                                           mempool.ref,
//                                           None,
//                                           settings)
//      )
//      val history: History = generateDummyHistory(testNetSettings)
//
//      val address: InetSocketAddress = new InetSocketAddress("0.0.0.0", 9000)
//      val peerHandler: TestProbe     = TestProbe()
//      val connectedPeer: ConnectedPeer = ConnectedPeer(
//        address,
//        peerHandler.ref,
//        Outgoing,
//        Handshake(protocolToBytes(testNetSettings.network.appVersion),
//                  "test node",
//                  Some(address),
//                  System.currentTimeMillis())
//      )
//
//      val timestamp1 = System.currentTimeMillis()
//      Thread.sleep(1000)
//      val timestamp2 = System.currentTimeMillis()
//
//      val header_first: Header = Header(
//        1.toByte,
//        ModifierId @@ Random.randomBytes(),
//        Digest32 @@ Random.randomBytes(),
//        timestamp2,
//        2,
//        scala.util.Random.nextLong(),
//        testNetSettings.constants.InitialDifficulty,
//        EquihashSolution(Seq(1, 3)),
//        Random.randomBytes()
//      )
//      val header_second: Header = Header(
//        1.toByte,
//        header_first.id,
//        Digest32 @@ Random.randomBytes(),
//        timestamp1,
//        1,
//        scala.util.Random.nextLong(),
//        testNetSettings.constants.InitialDifficulty,
//        EquihashSolution(Seq(1, 3)),
//        Random.randomBytes()
//      )
//
//      history.append(header_first)
//
//      nodeViewSync.send(downloadedModifiersValidator, UpdatedHistory(history))
//
//      /* Header */
//      val mods = Seq(header_second).map(x => x.id -> HeaderProtoSerializer.toProto(x).toByteArray.reverse).toMap
//      val msg  = ModifiersForValidating(connectedPeer, Header.modifierTypeId, mods)
//
//      deliveryManager.send(downloadedModifiersValidator, msg)
//      peersKeeper.expectMsg(BanPeer(connectedPeer, CorruptedSerializedBytes))
//      nodeViewHolder.expectNoMsg()
//      nodeViewSync.expectMsg(InvalidModifier(header_second.id))
//    }
//    "find corrupted payload" in {
//      val nodeViewHolder  = TestProbe()
//      val peersKeeper     = TestProbe()
//      val deliveryManager = TestProbe()
//      val nodeViewSync    = TestProbe()
//      val mempool         = TestProbe()
//
//      val address: InetSocketAddress = new InetSocketAddress("0.0.0.0", 9000)
//      val peerHandler: TestProbe     = TestProbe()
//      val connectedPeer: ConnectedPeer = ConnectedPeer(
//        address,
//        peerHandler.ref,
//        Outgoing,
//        Handshake(protocolToBytes(testNetSettings.network.appVersion),
//                  "test node",
//                  Some(address),
//                  System.currentTimeMillis())
//      )
//
//      val downloadedModifiersValidator = TestActorRef[DownloadedModifiersValidator](
//        DownloadedModifiersValidator.props(testNetSettings.constants.ModifierIdSize,
//                                           nodeViewHolder.ref,
//                                           peersKeeper.ref,
//                                           nodeViewSync.ref,
//                                           mempool.ref,
//                                           None,
//                                           settings)
//      )
//      val history: History = generateDummyHistory(testNetSettings)
//
//      val historyWith10Blocks = (0 until 10).foldLeft(history, Seq.empty[Block]) {
//        case ((prevHistory, blocks), _) =>
//          val block: Block = generateNextBlock(prevHistory)
//          prevHistory.append(block.header)
//          prevHistory.append(block.payload)
//          (prevHistory.reportModifierIsValid(block), blocks :+ block)
//      }
//
//      val payload = Payload(ModifierId @@ scorex.utils.Random.randomBytes(), Seq(coinbaseTransaction))
//
//      nodeViewSync.send(downloadedModifiersValidator, UpdatedHistory(historyWith10Blocks._1))
//
//      val bytes = PayloadProtoSerializer.toProto(payload).toByteArray
//
//      val mods: Map[ModifierId, Array[Byte]] = (historyWith10Blocks._2.map(
//        b => b.payload.id -> PayloadProtoSerializer.toProto(b.payload).toByteArray.reverse
//      ) :+ (payload.id -> bytes)).toMap
//
//      deliveryManager
//        .send(downloadedModifiersValidator, ModifiersForValidating(connectedPeer, Payload.modifierTypeId, mods))
//
//      peersKeeper.expectMsg(BanPeer(connectedPeer, CorruptedSerializedBytes))
//      nodeViewHolder.expectMsg(ModifierFromRemote(payload))
//    }
//  }
//
//  def generateBlocks(qty: Int, history: History): (History, List[Block]) =
//    (0 until qty).foldLeft(history, List.empty[Block]) {
//      case ((prevHistory, blocks), _) =>
//        val block: Block = generateNextBlock(prevHistory)
//        prevHistory.append(block.header)
//        prevHistory.append(block.payload)
//        val a = prevHistory.reportModifierIsValid(block)
//        (a, blocks :+ block)
//    }
//}
