package encry.view.fast.sync

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import akka.testkit.TestProbe
import encry.modifiers.InstanceFactory
import encry.network.PeerConnectionHandler.{ConnectedPeer, Incoming}
import encry.settings.TestNetSettings
import encry.storage.VersionalStorage.{StorageKey, StorageValue}
import encry.view.state.avlTree.utils.implicits.Instances._
import encry.view.state.avlTree.{AvlTree, NodeSerilalizer}
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.network.BasicMessagesRepo.Handshake
import org.scalatest.{Matchers, OneInstancePerTest, WordSpecLike}
import scorex.utils.Random
import FastSyncTestsUtils._
import SnapshotChunkProto.SnapshotChunkMessage
import encry.view.fast.sync.SnapshotHolder.{SnapshotChunkSerializer, SnapshotManifestSerializer}

class SnapshotDownloadControllerTest
    extends WordSpecLike
    with Matchers
    with InstanceFactory
    with OneInstancePerTest
    with TestNetSettings {
  implicit val system: ActorSystem = ActorSystem("SynchronousTestingSpec")

  "d" should {
    "f" in {
      val avl1: AvlTree[StorageKey, StorageValue] =
        createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 20)

      val chunks = AvlTree.getChunks(avl1.rootNode, 1, avl1.storage)

      val s = chunks.map { elem =>
       SnapshotChunkSerializer.toProto(elem).toByteArray
      }

      println(chunks.head)

      println("\n----------//////--------\n")

      val d = s.map { el =>
        SnapshotChunkSerializer.fromProto(SnapshotChunkMessage.parseFrom(el)).get
      }

      println(d.head)
    }
  }

//  "SnapshotDownloadController" should {
//    "process manifest function" should {
//      "correct height, correct manifest id, correct cp condition" in {
//        val (_, _, downloadController, blocks, manifest, history) = initializeTestState()
//
//        val correctController: SnapshotDownloadController = downloadController.copy(
//          requiredManifestHeight = blocks.last.header.height
//        )
//
//        val result =
//          correctController.processManifest(SnapshotManifestSerializer.toProto(manifest), createRemote(), history)
//
//        result.isRight shouldBe true
//        result.right.get._2.nonEmpty shouldBe true
//        result.right.get._1.notYetRequested.forall { id =>
//          manifest.chunksKeys.exists(_.sameElements(id))
//        } shouldBe true
//      }
//      "if some manifest is already in process - skip new one" in {
//        val (_, _, downloadController, blocks, manifest, history) = initializeTestState()
//
//        val correctDownloadController: SnapshotDownloadController =
//          downloadController.copy(
//            requiredManifestHeight = blocks.last.header.height,
//            cp = Some(createRemote("999"))
//          )
//
//        val res = correctDownloadController.processManifest(
//          SnapshotManifestSerializer.toProto(manifest),
//          createRemote(),
//          history
//        )
//
//        res.isRight shouldBe true
//        res.right.get._2.isEmpty shouldBe true
//      }
//      "if manifest has wrong id - ban sender(when in process manifest exists)" in {
//        val (_, _, downloadController, _, manifest, history) = initializeTestState()
//
//        val correctController = downloadController.copy(cp = Some(createRemote("999")))
//
//        val res = correctController.processManifest(
//          SnapshotManifestSerializer.toProto(manifest),
//          createRemote(),
//          history
//        )
//        res.isLeft shouldBe true
//      }
//      "if manifest has wrong id - ban sender(when in process manifest doesn't exist)" in {
//        val (_, _, downloadController, _, manifest, history) = initializeTestState()
//
//        val res = downloadController.processManifest(
//          SnapshotManifestSerializer.toProto(manifest),
//          createRemote(),
//          history
//        )
//        res.isLeft shouldBe true
//      }
//    }
//    "process request chunk function" should {
//      "validation success with correct peer and correct chunk" in {
//        val (_, processor, downloadController, _, manifest, _) = initializeTestState()
//
//        val correctDownloadController = downloadController
//          .copy(cp = Some(createRemote()),
//                requestedChunks =
//                  Set(ByteArrayWrapper(processor.getChunkById(manifest.chunksKeys.head).get.id.toByteArray)))
//
//        val result = correctDownloadController.processRequestedChunk(
//          processor.getChunkById(manifest.chunksKeys.head).get,
//          createRemote()
//        )
//
//        result.isRight shouldBe true
//        result.right.get._2.nonEmpty shouldBe true
//        result.right.get._1.requestedChunks.contains(ByteArrayWrapper(manifest.chunksKeys.head)) shouldBe false
//      }
//      "skip chunk from unknown peer" in {
//        val (_, processor, downloadController, _, manifest, _) = initializeTestState()
//
//        val result = downloadController.processRequestedChunk(
//          processor.getChunkById(manifest.chunksKeys.head).get,
//          createRemote(port = 3234)
//        )
//
//        result.isRight shouldBe true
//        result.right.get._2.isEmpty shouldBe true
//      }
//      "validation error with incorrect chunk" in {
//        val (_, processor, downloadController, _, manifest, _) = initializeTestState()
//
//        val snapshotDownloadController = downloadController
//          .copy(cp = Some(createRemote()))
//
//        val result = snapshotDownloadController.processRequestedChunk(
//          processor.getChunkById(manifest.chunksKeys.head).get.copy(id = null),
//          createRemote()
//        )
//
//        result.isLeft shouldBe true
//      }
//      "validation error with un-waited chunk" in {
//        val (_, processor, downloadController, _, manifest, _) = initializeTestState()
//
//        val snapshotDownloadController = downloadController
//          .copy(
//            cp = Some(createRemote()),
//            requestedChunks = Set(ByteArrayWrapper(processor.getChunkById(manifest.chunksKeys.last).get.id.toByteArray))
//          )
//
//        val result = snapshotDownloadController.processRequestedChunk(
//          processor.getChunkById(manifest.chunksKeys.head).get,
//          createRemote()
//        )
//
//        result.isLeft shouldBe true
//      }
//      "correctly process correct chunk" in {
//        val (_, processor, downloadController, _, manifest, _) = initializeTestState()
//
//        val snapshotDownloadController = downloadController
//          .copy(cp = Some(createRemote()),
//                requestedChunks =
//                  Set(ByteArrayWrapper(processor.getChunkById(manifest.chunksKeys.last).get.id.toByteArray)))
//
//        val result = snapshotDownloadController.processRequestedChunk(
//          processor.getChunkById(manifest.chunksKeys.last).get,
//          createRemote()
//        )
//
//        result.isRight shouldBe true
//        result.right.get._1.requestedChunks.isEmpty shouldBe true
//        result.right.get._2.nonEmpty shouldBe true
//        result.right.get._2.forall { node =>
//          processor.getChunkById(manifest.chunksKeys.last).get.chunks.exists { n =>
//            NodeSerilalizer
//              .fromProto[StorageKey, StorageValue](n)
//              .hash
//              .sameElements(NodeSerilalizer.fromProto[StorageKey, StorageValue](node).hash)
//          }
//        } shouldBe true
//      }
//    }
//    "process manifest has changed function" should {
//      "skip manifest if new is incorrect" in {
//        val (_, _, downloadController, _, manifest, history) = initializeTestState()
//
//        val res = downloadController.processManifestHasChangedMessage(
//          Random.randomBytes(),
//          SnapshotManifestSerializer.toProto(manifest),
//          history,
//          createRemote()
//        )
//
//        res.isLeft shouldBe true
//      }
//      "process if all conditions are correct" in {
//        val (_, _, downloadController, blocks, manifest, history) = initializeTestState()
//
//        val prevManifestId = Random.randomBytes()
//        val correctDownloadProcessor = downloadController.copy(
//          requiredManifestId = prevManifestId,
//          requiredManifestHeight = blocks.last.header.height
//        )
//
//        val res = correctDownloadProcessor.processManifestHasChangedMessage(
//          prevManifestId,
//          SnapshotManifestSerializer.toProto(manifest),
//          history,
//          createRemote()
//        )
//
//        res.isRight shouldBe true
//        res.right.get._2.nonEmpty shouldBe true
//        res.right.get._1.notYetRequested.forall { id =>
//          manifest.chunksKeys.exists(_.sameElements(id))
//        } shouldBe true
//      }
//    }
//    "process next request chunks message function" should {
//      "Request new chunks if it's needed" in {
//        val ids = (0 to 10).map(_ => Random.randomBytes()).toList
//        val snapshotDownloadController = SnapshotDownloadController
//          .empty(settings)
//          .copy(notYetRequested = ids)
//        val result = snapshotDownloadController.processRequestChunksMessage
//        result.isRight shouldBe true
//        result.right.get._2.nonEmpty shouldBe true
//        result.right.get._2.size == ids.size shouldBe true
//      }
//      "Set fast sync done correctly" in {
//        val (_, _, downloadController, _, manifest, _) = initializeTestState()
//
//        val snapshotDownloadController = downloadController
//          .copy(requiredManifestId = manifest.manifestId, cp = Some(createRemote()))
//
//        val result = snapshotDownloadController.processRequestChunksMessage
//        result.isLeft shouldBe true
//        result.left.get shouldBe true
//      }
//      "chunksIdsToDownload update needToBeRequested correctly" in {}
//      "chunksIdsToDownload set awaitingResponse correctly" in {}
//    }
//  }

  def createRemote(host: String = "111", port: Int = 999): ConnectedPeer = {
    val address = new InetSocketAddress(host, port)
    ConnectedPeer(
      address,
      TestProbe().ref,
      Incoming,
      Handshake(protocolToBytes(settings.network.appVersion), "111", Some(address), System.currentTimeMillis())
    )
  }
}
