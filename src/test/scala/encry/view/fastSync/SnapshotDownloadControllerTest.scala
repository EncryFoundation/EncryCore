package encry.view.fastSync

import java.net.InetSocketAddress
import akka.actor.ActorSystem
import akka.testkit.TestProbe
import encry.modifiers.InstanceFactory
import encry.network.DeliveryManagerTests.DMUtils
import encry.network.PeerConnectionHandler.{ ConnectedPeer, Incoming }
import encry.settings.TestNetSettings
import encry.storage.VersionalStorage.{ StorageKey, StorageValue }
import encry.view.fastSync.SnapshotHolder.SnapshotManifestSerializer
import encry.view.state.UtxoState
import encry.view.state.avlTree.utils.implicits.Instances._
import encry.view.state.avlTree.{ AvlTree, NodeSerilalizer }
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.network.BasicMessagesRepo.Handshake
import org.encryfoundation.common.utils.TaggedTypes.Height
import org.scalatest.{ Matchers, OneInstancePerTest, WordSpecLike }
import scorex.utils.Random
import FastSyncTestsUtils._
import org.encryfoundation.common.utils.Algos

class SnapshotDownloadControllerTest
    extends WordSpecLike
    with Matchers
    with InstanceFactory
    with OneInstancePerTest
    with TestNetSettings {
  implicit val system: ActorSystem = ActorSystem("SynchronousTestingSpec")

  "SnapshotDownloadController" should {
    "process manifest function" should {
      "process new valid manifest correctly" in {
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 20)
        val block1: Block = generateGenesisBlock(Height @@ 1)
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor
          .create(settings, tmpDir)
          .processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)

        val history              = generateDummyHistory(settings)
        val (historyNew, blocks) = DMUtils.generateBlocks(10, history)
        val lastBlock            = blocks.last
        val manifest = snapshotProcessor.bestPotentialManifest.get
          .copy(manifestId = Algos.hash(lastBlock.header.stateRoot ++ lastBlock.id))

        val snapshotDownloadController = SnapshotDownloadController
          .empty(settings)
          .copy(requiredManifestHeight = lastBlock.header.height)

        val res = snapshotDownloadController.processManifest(
          SnapshotManifestSerializer.toProto(manifest),
          createRemote(),
          historyNew
        )
        res.isRight shouldBe true
        res.right.get._2.nonEmpty shouldBe true
        res.right.get._1.notYetRequested.forall { id =>
          manifest.chunksKeys.exists(_.sameElements(id))
        } shouldBe true
      }
      "if some manifest is already in process - skip new one" in {
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 20)
        val block1: Block = generateGenesisBlock(Height @@ 1)
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor
          .create(settings, tmpDir)
          .processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)

        val history              = generateDummyHistory(settings)
        val (historyNew, blocks) = DMUtils.generateBlocks(10, history)
        val lastBlock            = blocks.last
        val manifest = snapshotProcessor.bestPotentialManifest.get
          .copy(manifestId = Algos.hash(lastBlock.header.stateRoot ++ lastBlock.id))

        val snapshotDownloadController = SnapshotDownloadController
          .empty(settings)
          .copy(cp = Some(createRemote("888")), requiredManifestHeight = lastBlock.header.height)

        val res = snapshotDownloadController.processManifest(
          SnapshotManifestSerializer.toProto(manifest),
          createRemote(),
          historyNew
        )
        res.isRight shouldBe true
        res.right.get._2.isEmpty shouldBe true
      }
      "if manifest has wrong id - ban sender(when in process manifest exists)" in {
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 20)
        val block1: Block = generateGenesisBlock(Height @@ 1)
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor
          .create(settings, tmpDir)
          .processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)

        val history              = generateDummyHistory(settings)
        val (historyNew, blocks) = DMUtils.generateBlocks(10, history)
        val manifest             = snapshotProcessor.bestPotentialManifest.get.copy(manifestId = blocks.head.header.stateRoot)

        val snapshotDownloadController = SnapshotDownloadController
          .empty(settings)
          .copy(cp = Some(createRemote("888")), requiredManifestHeight = blocks.last.header.height)

        val res = snapshotDownloadController.processManifest(
          SnapshotManifestSerializer.toProto(manifest),
          createRemote(),
          historyNew
        )
        res.isLeft shouldBe true
      }
      "if manifest has wrong id - ban sender(when in process manifest doesn't exist)" in {
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 20)
        val block1: Block = generateGenesisBlock(Height @@ 1)
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor
          .create(settings, tmpDir)
          .processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)

        val history              = generateDummyHistory(settings)
        val (historyNew, blocks) = DMUtils.generateBlocks(10, history)
        val manifest             = snapshotProcessor.bestPotentialManifest.get.copy(manifestId = blocks.head.header.stateRoot)

        val snapshotDownloadController = SnapshotDownloadController
          .empty(settings)

        val res = snapshotDownloadController.processManifest(
          SnapshotManifestSerializer.toProto(manifest),
          createRemote(),
          historyNew
        )
        res.isLeft shouldBe true
      }
    }
    "process request chunk function" should {
      "skip chunk from unknown peer" in {
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 20)
        val block1: Block = generateGenesisBlock(Height @@ 1)
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor
          .create(settings, tmpDir)
          .processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)

        val history     = generateDummyHistory(settings)
        val (_, blocks) = DMUtils.generateBlocks(10, history)
        val manifest    = snapshotProcessor.bestPotentialManifest.get.copy(manifestId = blocks.last.header.stateRoot)

        val snapshotDownloadController = SnapshotDownloadController
          .empty(settings)
          .copy(cp = Some(createRemote()))

        val result = snapshotDownloadController.processRequestedChunk(
          snapshotProcessor.getChunkById(manifest.chunksKeys.head).get,
          createRemote(port = 3234)
        )

        result.isRight shouldBe true
        result.right.get._2.isEmpty shouldBe true
      }
      "process chunk from correct peer" in {
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 20)
        val block1: Block = generateGenesisBlock(Height @@ 1)
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor
          .create(settings, tmpDir)
          .processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)

        val history     = generateDummyHistory(settings)
        val (_, blocks) = DMUtils.generateBlocks(10, history)
        val manifest = snapshotProcessor.bestPotentialManifest.get
          .copy(manifestId = Algos.hash(blocks.last.header.stateRoot ++ blocks.last.id))

        val snapshotDownloadController = SnapshotDownloadController
          .empty(settings)
          .copy(cp = Some(createRemote()),
                requestedChunks =
                  Set(ByteArrayWrapper(snapshotProcessor.getChunkById(manifest.chunksKeys.head).get.id.toByteArray)))

        val result = snapshotDownloadController.processRequestedChunk(
          snapshotProcessor.getChunkById(manifest.chunksKeys.head).get,
          createRemote()
        )

        result.isRight shouldBe true
        result.right.get._2.nonEmpty shouldBe true
      }
      "ban peer with incorrect chunk" in {
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 20)
        val block1: Block = generateGenesisBlock(Height @@ 1)
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor
          .create(settings, tmpDir)
          .processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)

        val history     = generateDummyHistory(settings)
        val (_, blocks) = DMUtils.generateBlocks(10, history)
        val manifest    = snapshotProcessor.bestPotentialManifest.get.copy(manifestId = blocks.last.header.stateRoot)

        val snapshotDownloadController = SnapshotDownloadController
          .empty(settings)
          .copy(cp = Some(createRemote()))

        val result = snapshotDownloadController.processRequestedChunk(
          snapshotProcessor.getChunkById(manifest.chunksKeys.head).get.copy(id = null),
          createRemote()
        )

        result.isLeft shouldBe true
      }
      "skip chunk which id is not awaited" in {
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 20)
        val block1: Block = generateGenesisBlock(Height @@ 1)
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor
          .create(settings, tmpDir)
          .processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)

        val history     = generateDummyHistory(settings)
        val (_, blocks) = DMUtils.generateBlocks(10, history)
        val manifest    = snapshotProcessor.bestPotentialManifest.get.copy(manifestId = blocks.last.header.stateRoot)

        val snapshotDownloadController = SnapshotDownloadController
          .empty(settings)
          .copy(
            cp = Some(createRemote()),
            requestedChunks =
              Set(ByteArrayWrapper(snapshotProcessor.getChunkById(manifest.chunksKeys.head).get.id.toByteArray))
          )

        val result = snapshotDownloadController.processRequestedChunk(
          snapshotProcessor.getChunkById(manifest.chunksKeys.head).get,
          createRemote()
        )

        result.isRight shouldBe true
      }
      "correctly process correct chunk" in {
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 20)
        val block1: Block = generateGenesisBlock(Height @@ 1)
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor
          .create(settings, tmpDir)
          .processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)

        val history     = generateDummyHistory(settings)
        val (_, blocks) = DMUtils.generateBlocks(10, history)
        val manifest = snapshotProcessor.bestPotentialManifest.get
          .copy(manifestId = Algos.hash(blocks.last.header.stateRoot ++ blocks.last.id))

        val snapshotDownloadController = SnapshotDownloadController
          .empty(settings)
          .copy(cp = Some(createRemote()),
                requestedChunks =
                  Set(ByteArrayWrapper(snapshotProcessor.getChunkById(manifest.chunksKeys.last).get.id.toByteArray)))

        val result = snapshotDownloadController.processRequestedChunk(
          snapshotProcessor.getChunkById(manifest.chunksKeys.last).get,
          createRemote()
        )

        result.isRight shouldBe true
        result.right.get._1.requestedChunks.isEmpty shouldBe true
        result.right.get._2.nonEmpty shouldBe true
        result.right.get._2.forall { node =>
          snapshotProcessor.getChunkById(manifest.chunksKeys.last).get.chunks.exists { n =>
            NodeSerilalizer
              .fromProto[StorageKey, StorageValue](n)
              .hash
              .sameElements(NodeSerilalizer.fromProto[StorageKey, StorageValue](node).hash)
          }
        } shouldBe true
      }
    }
    "process manifest has changed function" should {
      "skip manifest if new is incorrect" in {
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 20)
        val block1: Block = generateGenesisBlock(Height @@ 1)
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor
          .create(settings, tmpDir)
          .processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)

        val history              = generateDummyHistory(settings)
        val (historyNew, blocks) = DMUtils.generateBlocks(10, history)
        val lastBlock            = blocks.last
        val manifest             = snapshotProcessor.bestPotentialManifest.get.copy(manifestId = lastBlock.header.stateRoot)

        val snapshotDownloadController = SnapshotDownloadController
          .empty(settings)
          .copy(requiredManifestHeight = lastBlock.header.height)

        val res = snapshotDownloadController.processManifestHasChangedMessage(
          Random.randomBytes(),
          SnapshotManifestSerializer.toProto(manifest),
          historyNew,
          createRemote()
        )

        res.isLeft shouldBe true
      }
      "process if all conditions are correct" in {
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 20)
        val block1: Block = generateGenesisBlock(Height @@ 1)
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor
          .create(settings, tmpDir)
          .processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)

        val history              = generateDummyHistory(settings)
        val (historyNew, blocks) = DMUtils.generateBlocks(10, history)
        val lastBlock            = blocks.last
        val manifest             = snapshotProcessor.bestPotentialManifest.get.copy(manifestId = lastBlock.header.stateRoot)

        val snapshotDownloadController = SnapshotDownloadController
          .empty(settings)
          .copy(requiredManifestHeight = lastBlock.header.height)

        val res = snapshotDownloadController.processManifestHasChangedMessage(
          Random.randomBytes(),
          SnapshotManifestSerializer.toProto(manifest),
          historyNew,
          createRemote()
        )

        res

//        val result: SnapshotDownloadController.ProcessRequestedManifestResult =
//          snapshotDownloadController.processManifest(SnapshotManifestSerializer.toProto(correctManifest),
//                                                     createRemote(),
//                                                     historyNew,
//                                                     manifestHasChangedMessage = true,
//                                                     manifest.ManifestId)
//        result.isForBan shouldBe false
//        result.startRequestChunks shouldBe true
//        result.controller.currentManifest.exists(_.ManifestId.sameElements(correctManifest.ManifestId)) shouldBe true
      }
    }
    "process next request chunks message function" should {
      "Request new chunks if it's needed" in {
        val ids = (0 to 10).map(_ => Random.randomBytes()).toList
        val snapshotDownloadController = SnapshotDownloadController
          .empty(settings)
          .copy(notYetRequested = ids)
        val result = snapshotDownloadController.processNextRequestChunksMessage
        result.isRight shouldBe true
        result.right.get._2.nonEmpty shouldBe true
        result.right.get._2.size == ids.size shouldBe true
      }
      "Set fast sync done correctly" in {
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 20)
        val block1: Block = generateGenesisBlock(Height @@ 1)
        val processor1    = snapshotProcessor.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)

        val manifest = processor1.bestPotentialManifest.get
        val snapshotDownloadController = SnapshotDownloadController
          .empty(settings)
          .copy(requiredManifestId = manifest.manifestId, cp = Some(createRemote()))

        val result = snapshotDownloadController.processNextRequestChunksMessage
        result.isLeft shouldBe true
        result.left.get shouldBe true
      }
      "Skip iteration correctly" in {
        val snapshotDownloadController = SnapshotDownloadController
          .empty(settings)
          .copy(requestedChunks = Set(ByteArrayWrapper(Random.randomBytes())))
        val result = snapshotDownloadController.processNextRequestChunksMessage
        result.isLeft shouldBe true
        result.left.get shouldBe false
      }
      "chunksIdsToDownload update needToBeRequested correctly" in {}
      "chunksIdsToDownload set awaitingResponse correctly" in {}
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
