package encry.view.fast.sync

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import encry.modifiers.InstanceFactory
import encry.network.PeerConnectionHandler.{ConnectedPeer, Incoming}
import encry.settings.TestNetSettings
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.network.BasicMessagesRepo.Handshake
import org.scalatest.{Matchers, OneInstancePerTest, WordSpecLike}
import scorex.utils.Random
import FastSyncTestsUtils._
import encry.view.fast.sync.SnapshotHolder.{SnapshotChunkSerializer, SnapshotManifestSerializer}

class SnapshotDownloadControllerTest extends TestKit(ActorSystem("SynchronousTestingSpec"))
    with WordSpecLike
    with Matchers
    with InstanceFactory
    with OneInstancePerTest
    with TestNetSettings {

  "SnapshotDownloadController" should {

    "process manifest function" should {

      "correct height, correct manifest id, correct cp condition" in {
        val (_, _, downloadController, blocks, manifest, history) = initializeTestState()

        val correctController: SnapshotDownloadController = downloadController.copy(
          requiredManifestHeight = blocks.last.header.height
        )

        val result =
          correctController.processManifest(SnapshotManifestSerializer.toProto(manifest), createRemote(), history)

        result.isRight shouldBe true
        result.right.get.notYetRequested.forall { id =>
          manifest.chunksKeys.exists(_.sameElements(id))
        } shouldBe true
      }

    }
    "process request chunk function" should {

      "validation success with correct peer and correct chunk" in {

        val (_, processor, downloadController, _, manifest, _) = initializeTestState()

        val correctDownloadController = downloadController
          .copy(cp = Some(createRemote()),
                requestedChunks =
                  Set(ByteArrayWrapper(processor.getChunkById(manifest.chunksKeys.head).get.id.toByteArray)))

        val result = correctDownloadController.processRequestedChunk(
          processor.getChunkById(manifest.chunksKeys.head).get,
          createRemote()
        )

        result.isRight shouldBe true
        result.right.get._1.requestedChunks.contains(ByteArrayWrapper(manifest.chunksKeys.head)) shouldBe false
      }

      "skip chunk from unknown peer" in {

        val (_, processor, downloadController, _, manifest, _) = initializeTestState()

        val result = downloadController.copy(requestedChunks = Set(ByteArrayWrapper(manifest.chunksKeys.head))).processRequestedChunk(
          processor.getChunkById(manifest.chunksKeys.head).get,
          createRemote(port = 3234)
        )

        result.isRight shouldBe true
      }

      "validation error with incorrect chunk" in {

        val (_, processor, downloadController, _, manifest, _) = initializeTestState()

        val snapshotDownloadController = downloadController
          .copy(cp = Some(createRemote()))

        val result = snapshotDownloadController.processRequestedChunk(
          processor.getChunkById(manifest.chunksKeys.head).get.copy(id = null),
          createRemote()
        )

        result.isLeft shouldBe true
      }

      "validation error with un-waited chunk" in {

        val (_, processor, downloadController, _, manifest, _) = initializeTestState()

        val snapshotDownloadController = downloadController
          .copy(
            cp = Some(createRemote()),
            requestedChunks = Set.empty
          )

        val result = snapshotDownloadController.processRequestedChunk(
          processor.getChunkById(manifest.chunksKeys.head).get,
          createRemote()
        )

        result.isLeft shouldBe true
      }

      "correctly process correct chunk" in {
        val (_, processor, downloadController, _, manifest, _) = initializeTestState()

        val snapshotDownloadController = downloadController
          .copy(cp = Some(createRemote()),
                requestedChunks =
                  Set(ByteArrayWrapper(processor.getChunkById(manifest.chunksKeys.last).get.id.toByteArray)))

        val result = snapshotDownloadController.processRequestedChunk(
          processor.getChunkById(manifest.chunksKeys.last).get,
          createRemote()
        )

        result.isRight shouldBe true
        result.right.get._1.requestedChunks.isEmpty shouldBe true
        result.right.get._2.id.sameElements(SnapshotChunkSerializer.fromProto(processor.getChunkById(manifest.chunksKeys.last).get).get.id) shouldBe true
      }
    }

    "process manifest has changed function" should {

      "process if all conditions are correct" in {
        val (_, _, downloadController, blocks, manifest, history) = initializeTestState()

        val prevManifestId = Random.randomBytes()
        val correctDownloadProcessor = downloadController.copy(
          requiredManifestId = prevManifestId,
          requiredManifestHeight = blocks.last.header.height
        )

        val res = correctDownloadProcessor.processManifestHasChangedMessage(
          SnapshotManifestSerializer.toProto(manifest.copy(manifestId = prevManifestId)),
          createRemote()
        )

        res.isRight shouldBe true
        res.right.get.notYetRequested.forall { id =>
          manifest.chunksKeys.exists(_.sameElements(id))
        } shouldBe true
      }
    }

  }

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
