package encry.view.fastSync

import java.io.File
import java.net.InetSocketAddress
import akka.actor.ActorSystem
import akka.testkit.TestProbe
import encry.modifiers.InstanceFactory
import encry.network.DeliveryManagerTests.DMUtils
import encry.network.PeerConnectionHandler.{ ConnectedPeer, Incoming }
import encry.settings.TestNetSettings
import encry.storage.VersionalStorage.{ StorageKey, StorageValue, StorageVersion }
import encry.storage.levelDb.versionalLevelDB.{ LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion }
import encry.utils.FileHelper
import encry.view.fastSync.SnapshotHolder.SnapshotManifestSerializer
import encry.view.state.UtxoState
import encry.view.state.avlTree.utils.implicits.Instances._
import encry.view.state.avlTree.{ AvlTree, NodeSerilalizer }
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.network.BasicMessagesRepo.Handshake
import org.encryfoundation.common.utils.TaggedTypes.{ Height, ModifierId }
import org.iq80.leveldb.Options
import org.scalatest.{ Matchers, OneInstancePerTest, WordSpecLike }
import scorex.utils.Random

class SnapshotDownloadControllerTest
    extends WordSpecLike
    with Matchers
    with InstanceFactory
    with OneInstancePerTest
    with TestNetSettings {
  implicit val system: ActorSystem = ActorSystem("SynchronousTestingSpec")

  "SnapshotDownloadController" should {
    "process manifest function(new manifest)" should {
      "process new manifest with correct root hash correctly" in {
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 20)
        val block1: Block = generateGenesisBlock(Height @@ 1)
        val processor1    = snapshotProcessor.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)

        val manifest                   = processor1.bestPotentialManifest.get
        val snapshotDownloadController = SnapshotDownloadController.empty(settings)
        val history                    = generateDummyHistory(settings)
        val (historyNew, blocks)       = DMUtils.generateBlocks(10, history)
        val lastBlock                  = blocks.last
        val correctManifest = manifest.copy(
          bestBlockId = lastBlock.id,
          rootHash = lastBlock.header.stateRoot,
          bestBlockHeight = lastBlock.header.height
        )

        val result: SnapshotDownloadController.ProcessRequestedManifestResult =
          snapshotDownloadController.processManifest(SnapshotManifestSerializer.toProto(correctManifest),
                                                     createRemote(),
                                                     historyNew)
        result.isForBan shouldBe false
        result.startRequestChunks shouldBe true
        result.controller.currentManifest.get.ManifestId.sameElements(correctManifest.ManifestId) shouldBe true
      }
      "if some manifest is already in process - skip new one" in {
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 20)
        val block1: Block = generateGenesisBlock(Height @@ 1)
        val processor1    = snapshotProcessor.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)

        val manifest = processor1.bestPotentialManifest.get
        val snapshotDownloadController = SnapshotDownloadController
          .empty(settings)
          .copy(currentManifest = Some(manifest))
        val history              = generateDummyHistory(settings)
        val (historyNew, blocks) = DMUtils.generateBlocks(10, history)
        val lastBlock            = blocks.last
        val correctManifest = manifest.copy(
          bestBlockId = lastBlock.id,
          rootHash = lastBlock.header.stateRoot,
          bestBlockHeight = lastBlock.header.height
        )

        val result: SnapshotDownloadController.ProcessRequestedManifestResult =
          snapshotDownloadController.processManifest(SnapshotManifestSerializer.toProto(correctManifest),
                                                     createRemote(),
                                                     historyNew)

        result.isForBan shouldBe false
        result.startRequestChunks shouldBe false
      }
      "if manifest has wrong root hash - ban sender(when in process manifest exists)" in {
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 20)
        val block1: Block = generateGenesisBlock(Height @@ 1)
        val processor1    = snapshotProcessor.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)

        val manifest = processor1.bestPotentialManifest.get
        val snapshotDownloadController = SnapshotDownloadController
          .empty(settings)
          .copy(currentManifest = Some(manifest))
        val history              = generateDummyHistory(settings)
        val (historyNew, blocks) = DMUtils.generateBlocks(10, history)
        val lastBlock            = blocks.last
        val correctManifest = manifest.copy(
          bestBlockId = lastBlock.id,
          bestBlockHeight = lastBlock.header.height
        )

        val result: SnapshotDownloadController.ProcessRequestedManifestResult =
          snapshotDownloadController.processManifest(SnapshotManifestSerializer.toProto(correctManifest),
                                                     createRemote(),
                                                     historyNew)

        result.isForBan shouldBe true
        result.startRequestChunks shouldBe false
      }
      "if manifest has wrong root hash - ban sender(when in process manifest doesn't exist)" in {
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 20)
        val block1: Block = generateGenesisBlock(Height @@ 1)
        val processor1 =
          snapshotProcessor.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)

        val manifest = processor1.bestPotentialManifest.get
        val snapshotDownloadController = SnapshotDownloadController
          .empty(settings)
        val history              = generateDummyHistory(settings)
        val (historyNew, blocks) = DMUtils.generateBlocks(10, history)
        val lastBlock            = blocks.last
        val correctManifest = manifest.copy(
          bestBlockId = lastBlock.id,
          bestBlockHeight = lastBlock.header.height
        )

        val result: SnapshotDownloadController.ProcessRequestedManifestResult =
          snapshotDownloadController.processManifest(SnapshotManifestSerializer.toProto(correctManifest),
                                                     createRemote(),
                                                     historyNew)

        result.isForBan shouldBe true
        result.startRequestChunks shouldBe false
      }
      "correctly create function result with correct manifest" in {
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 20)
        val block1: Block = generateGenesisBlock(Height @@ 1)
        val processor1    = snapshotProcessor.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)

        val manifest                   = processor1.bestPotentialManifest.get
        val snapshotDownloadController = SnapshotDownloadController.empty(settings)
        val history                    = generateDummyHistory(settings)
        val (historyNew, blocks)       = DMUtils.generateBlocks(10, history)
        val lastBlock                  = blocks.last
        val correctManifest = manifest.copy(
          bestBlockId = lastBlock.id,
          rootHash = lastBlock.header.stateRoot,
          bestBlockHeight = lastBlock.header.height
        )

        val result: SnapshotDownloadController.ProcessRequestedManifestResult =
          snapshotDownloadController.processManifest(SnapshotManifestSerializer.toProto(correctManifest),
                                                     createRemote(),
                                                     historyNew)
        result.controller.currentManifest.get.ManifestId.sameElements(correctManifest.ManifestId) shouldBe true
        result.controller.needToBeRequested.forall { id =>
          correctManifest.chunksKeys.exists(_.sameElements(id))
        } shouldBe true
        result.controller.awaitingResponse.isEmpty shouldBe true
      }
    }
    "process request chunk function" should {
      "skip chunk from unknown peer" in {
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 20)
        val block1: Block = generateGenesisBlock(Height @@ 1)
        val processor1    = snapshotProcessor.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)

        val manifest = processor1.bestPotentialManifest.get
        val snapshotDownloadController = SnapshotDownloadController
          .empty(settings)
          .copy(cp = Some(createRemote()))
        val history     = generateDummyHistory(settings)
        val (_, blocks) = DMUtils.generateBlocks(10, history)
        val lastBlock   = blocks.last
        val correctManifest = manifest.copy(
          bestBlockId = lastBlock.id,
          rootHash = lastBlock.header.stateRoot,
          bestBlockHeight = lastBlock.header.height
        )

        val result = snapshotDownloadController.processRequestedChunk(
          processor1.getChunkById(correctManifest.chunksKeys.head).get,
          createRemote(port = 3234)
        )

        result.isForBan shouldBe false
        result.newNodes.isEmpty shouldBe true
      }
      "process chunk from correct peer" in {
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 20)
        val block1: Block = generateGenesisBlock(Height @@ 1)
        val processor1    = snapshotProcessor.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)

        val manifest = processor1.bestPotentialManifest.get
        val snapshotDownloadController = SnapshotDownloadController
          .empty(settings)
          .copy(cp = Some(createRemote()), currentManifest = Some(manifest))
        val history     = generateDummyHistory(settings)
        val (_, blocks) = DMUtils.generateBlocks(10, history)
        val lastBlock   = blocks.last
        val correctManifest = manifest.copy(
          bestBlockId = lastBlock.id,
          rootHash = lastBlock.header.stateRoot,
          bestBlockHeight = lastBlock.header.height
        )

        val result = snapshotDownloadController.processRequestedChunk(
          processor1.getChunkById(correctManifest.chunksKeys.head).get,
          createRemote()
        )

        result.isForBan shouldBe false
        result.newNodes.isEmpty shouldBe true
      }
      "ban peer with incorrect chunk(wrong manifest id)" in {
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 20)
        val block1: Block = generateGenesisBlock(Height @@ 1)
        val processor1    = snapshotProcessor.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)

        val manifest = processor1.bestPotentialManifest.get
        val snapshotDownloadController = SnapshotDownloadController
          .empty(settings)
          .copy(cp = Some(createRemote()))
        val history     = generateDummyHistory(settings)
        val (_, blocks) = DMUtils.generateBlocks(10, history)
        val lastBlock   = blocks.last
        val correctManifest = manifest.copy(
          bestBlockId = lastBlock.id,
          rootHash = lastBlock.header.stateRoot,
          bestBlockHeight = lastBlock.header.height
        )

        val result = snapshotDownloadController.processRequestedChunk(
          processor1.getChunkById(correctManifest.chunksKeys.head).get,
          createRemote()
        )

        result.isForBan shouldBe true
        result.newNodes.isEmpty shouldBe true
      }
      "skip chunk which id is not awaited" in {
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 20)
        val block1: Block = generateGenesisBlock(Height @@ 1)
        val processor1    = snapshotProcessor.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)

        val manifest = processor1.bestPotentialManifest.get
        val snapshotDownloadController = SnapshotDownloadController
          .empty(settings)
          .copy(cp = Some(createRemote()), currentManifest = Some(manifest))
        val history     = generateDummyHistory(settings)
        val (_, blocks) = DMUtils.generateBlocks(10, history)
        val lastBlock   = blocks.last
        val correctManifest = manifest.copy(
          bestBlockId = lastBlock.id,
          rootHash = lastBlock.header.stateRoot,
          bestBlockHeight = lastBlock.header.height
        )

        val result = snapshotDownloadController.processRequestedChunk(
          processor1.getChunkById(correctManifest.chunksKeys.head).get,
          createRemote()
        )

        result.isForBan shouldBe false
        result.newNodes.isEmpty shouldBe true
      }
      "correctly process correct chunk" in {
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 20)
        val block1: Block = generateGenesisBlock(Height @@ 1)
        val processor1    = snapshotProcessor.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)

        val manifest = processor1.bestPotentialManifest.get
        val snapshotDownloadController = SnapshotDownloadController
          .empty(settings)
          .copy(
            cp = Some(createRemote()),
            currentManifest = Some(manifest),
            awaitingResponse =
              Set(ByteArrayWrapper(processor1.getChunkById(manifest.chunksKeys.last).get.id.toByteArray))
          )
        val history     = generateDummyHistory(settings)
        val (_, blocks) = DMUtils.generateBlocks(10, history)
        val lastBlock   = blocks.last
        val correctManifest = manifest.copy(
          bestBlockId = lastBlock.id,
          rootHash = lastBlock.header.stateRoot,
          bestBlockHeight = lastBlock.header.height
        )

        val result = snapshotDownloadController.processRequestedChunk(
          processor1.getChunkById(correctManifest.chunksKeys.last).get,
          createRemote()
        )

        result.isForBan shouldBe false
        result.controller.awaitingResponse.isEmpty shouldBe true
        result.newNodes.nonEmpty shouldBe true
        result.newNodes.forall { node =>
          processor1.getChunkById(correctManifest.chunksKeys.last).get.chunks.exists { n =>
            NodeSerilalizer
              .fromProto[StorageKey, StorageValue](n)
              .hash
              .sameElements(NodeSerilalizer.fromProto[StorageKey, StorageValue](node).hash)
          }
        } shouldBe true
      }
    }
    "process manifest function(manifest has changed)" should {
      "skip manifest if previous manifest id is incorrect" in {
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 20)
        val block1: Block = generateGenesisBlock(Height @@ 1)
        val processor1    = snapshotProcessor.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)

        val manifest = processor1.bestPotentialManifest.get
        val snapshotDownloadController = SnapshotDownloadController
          .empty(settings)
          .copy(currentManifest = Some(manifest))
        val history              = generateDummyHistory(settings)
        val (historyNew, blocks) = DMUtils.generateBlocks(10, history)
        val lastBlock            = blocks.last
        val correctManifest = manifest.copy(
          bestBlockId = lastBlock.id,
          rootHash = lastBlock.header.stateRoot,
          bestBlockHeight = lastBlock.header.height
        )

        val result: SnapshotDownloadController.ProcessRequestedManifestResult =
          snapshotDownloadController.processManifest(SnapshotManifestSerializer.toProto(correctManifest),
                                                     createRemote(),
                                                     historyNew,
                                                     manifestHasChangedMessage = true,
                                                     Random.randomBytes())
        result.isForBan shouldBe false
        result.startRequestChunks shouldBe false
      }
      "skip if sender is incorrect" in {
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 20)
        val block1: Block = generateGenesisBlock(Height @@ 1)
        val processor1    = snapshotProcessor.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)

        val manifest = processor1.bestPotentialManifest.get
        val snapshotDownloadController = SnapshotDownloadController
          .empty(settings)
          .copy(currentManifest = Some(manifest), cp = Some(createRemote()))
        val history              = generateDummyHistory(settings)
        val (historyNew, blocks) = DMUtils.generateBlocks(10, history)
        val lastBlock            = blocks.last
        val correctManifest = manifest.copy(
          bestBlockId = lastBlock.id,
          rootHash = lastBlock.header.stateRoot,
          bestBlockHeight = lastBlock.header.height
        )

        val result: SnapshotDownloadController.ProcessRequestedManifestResult =
          snapshotDownloadController.processManifest(SnapshotManifestSerializer.toProto(correctManifest),
                                                     createRemote(port = 1234),
                                                     historyNew,
                                                     manifestHasChangedMessage = true,
                                                     Random.randomBytes())
        result.isForBan shouldBe false
        result.startRequestChunks shouldBe false
      }
      "process if all conditions are correct" in {
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 20)
        val block1: Block = generateGenesisBlock(Height @@ 1)
        val processor1    = snapshotProcessor.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)

        val manifest = processor1.bestPotentialManifest.get
        val snapshotDownloadController = SnapshotDownloadController
          .empty(settings)
          .copy(currentManifest = Some(manifest))
        val history              = generateDummyHistory(settings)
        val (historyNew, blocks) = DMUtils.generateBlocks(10, history)
        val lastBlock            = blocks.last
        val correctManifest = manifest.copy(
          bestBlockId = ModifierId @@ Random.randomBytes(),
          rootHash = lastBlock.header.stateRoot,
          bestBlockHeight = lastBlock.header.height
        )

        val result: SnapshotDownloadController.ProcessRequestedManifestResult =
          snapshotDownloadController.processManifest(SnapshotManifestSerializer.toProto(correctManifest),
                                                     createRemote(),
                                                     historyNew,
                                                     manifestHasChangedMessage = true,
                                                     manifest.ManifestId)
        result.isForBan shouldBe false
        result.startRequestChunks shouldBe true
        result.controller.currentManifest.exists(_.ManifestId.sameElements(correctManifest.ManifestId)) shouldBe true
      }
    }
    "process next request chunks message function" should {
      "Request new chunks if it's needed" in {
        val ids = (0 to 10).map(_ => Random.randomBytes()).toList
        val snapshotDownloadController = SnapshotDownloadController.empty(settings)
          .copy(needToBeRequested = ids)
        val result: SnapshotDownloadController.ProcessNextRequestChunksMessageResult =
          snapshotDownloadController.processNextRequestChunksMessage()
        result.isSyncDone shouldBe false
        result.chunksToRequest.nonEmpty shouldBe true
        result.chunksToRequest.size == ids.size shouldBe true
      }
      "Set fast sync done correctly" in {
        val snapshotProcessor: SnapshotProcessor = SnapshotProcessor.create(settings, tmpDir)
        val avl1: AvlTree[StorageKey, StorageValue] =
          createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 20)
        val block1: Block = generateGenesisBlock(Height @@ 1)
        val processor1    = snapshotProcessor.processNewSnapshot(UtxoState(avl1, Height @@ 0, settings.constants), block1)

        val manifest                   = processor1.bestPotentialManifest.get
        val snapshotDownloadController = SnapshotDownloadController.empty(settings)
          .copy(currentManifest = Some(manifest))

        val result: SnapshotDownloadController.ProcessNextRequestChunksMessageResult =
          snapshotDownloadController.processNextRequestChunksMessage()
        result.isSyncDone shouldBe true
        result.chunksToRequest.isEmpty shouldBe true
      }
      "Skip iteration correctly" in {
        val snapshotDownloadController = SnapshotDownloadController.empty(settings)
          .copy(awaitingResponse = Set(ByteArrayWrapper(Random.randomBytes())))
        val result: SnapshotDownloadController.ProcessNextRequestChunksMessageResult =
          snapshotDownloadController.processNextRequestChunksMessage()
        result.isSyncDone shouldBe false
        result.chunksToRequest.isEmpty shouldBe true
      }
      "chunksIdsToDownload update needToBeRequested correctly" in {}
      "chunksIdsToDownload set awaitingResponse correctly" in {}
    }
  }

  def createAvl(address: String, from: Int, to: Int): AvlTree[StorageKey, StorageValue] = {
    val firstDir: File = FileHelper.getRandomTempDir
    val firstStorage: VLDBWrapper = {
      val levelDBInit = LevelDbFactory.factory.open(firstDir, new Options)
      VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB, keySize = 32))
    }
    val boxes: IndexedSeq[(StorageKey, StorageValue)] = (from to to)
      .map(i => genAssetBox(address, i, nonce = i))
      .map(bx => (StorageKey !@@ bx.id, StorageValue @@ bx.bytes))

    val firstAvl: AvlTree[StorageKey, StorageValue] = AvlTree[StorageKey, StorageValue](firstStorage)
    firstAvl
      .insertAndDeleteMany(
        StorageVersion @@ Random.randomBytes(),
        boxes.toList,
        List.empty
      )

  }

  def tmpDir: File = FileHelper.getRandomTempDir

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
