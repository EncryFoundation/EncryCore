package encry.view.fast.sync

import com.typesafe.scalalogging.StrictLogging
import encry.modifiers.InstanceFactory
import encry.settings.TestNetSettings
import SnapshotHolder.SnapshotChunkSerializer
import encry.view.state.avlTree.InternalNode
import org.scalatest.{Matchers, OneInstancePerTest, WordSpecLike}
import encry.view.state.avlTree.utils.implicits.Instances._

import cats.syntax.either._
import scala.util.{ Random => random }
import encry.view.fast.sync.FastSyncExceptions.{ApplicableChunkIsAbsent, FastSyncException}
import encry.view.fast.sync.FastSyncTestsUtils._

class SubtreesAssemblerTest
    extends WordSpecLike
    with Matchers
    with InstanceFactory
    with OneInstancePerTest
    with TestNetSettings
    with StrictLogging {

  "Trees validator" should {

    "check tree assembly(with correct nodes) with non-default chunk height" in {

      val (_, processor, _, _, manifest, history) = initializeTestState(chunkHeight = 3)

      val sn = settings
        .copy(
          levelDB = settings.levelDB.copy(maxVersions = 5),
          snapshotSettings = settings.snapshotSettings.copy(newSnapshotCreationHeight = 10)
        )

      val chunks = manifest
        .chunksKeys
        .flatMap(processor.getChunkById)
        .flatMap(SnapshotChunkSerializer.fromProto(_).toOption)

      val secondProcessor: SnapshotProcessor =
        SnapshotProcessor
          .create(sn, tmpDir)
          .initializeApplicableChunksCache(history, 80).right.get

      chunks.foldLeft(secondProcessor) { case (proc, nextChunk) =>
        val wUpdatedCache = proc.updateCache(nextChunk)
        wUpdatedCache.processNextApplicableChunk(wUpdatedCache).leftFlatMap {
            case e: ApplicableChunkIsAbsent => e.processor.asRight[FastSyncException]
            case t                          => t.asLeft[SnapshotProcessor]
        }.right.get
      }.assembleUTXOState.right.get.tree.selfInspectionAfterFastSync shouldBe true
    }

    "check tree assembly(with incorrect nodes) with non-default chunk height" in {
      val (_, processor, _, _, manifest, history) = initializeTestState(chunkHeight = 3)

      val sn = settings
        .copy(
          levelDB = settings.levelDB.copy(maxVersions = 5),
          snapshotSettings = settings.snapshotSettings.copy(newSnapshotCreationHeight = 10)
        )

      val chunks = manifest
        .chunksKeys
        .flatMap(processor.getChunkById)
        .flatMap(SnapshotChunkSerializer.fromProto(_).toOption)

      val secondProcessor: SnapshotProcessor =
        SnapshotProcessor
          .create(sn, tmpDir)
          .initializeApplicableChunksCache(history, 80).right.get

      chunks.foldLeft(secondProcessor) { case (proc, nextChunk) =>
        val node = nextChunk.node match {
          case l@InternalNode(_, _, _, _, _, _) =>
            l.copy(height = random.nextInt())
          case l => l
        }
        val wUpdatedCache = proc.updateCache(nextChunk.copy(node = node))
        wUpdatedCache.processNextApplicableChunk(wUpdatedCache).leftFlatMap {
          case e: ApplicableChunkIsAbsent => e.processor.asRight[FastSyncException]
          case t                          => t.asLeft[SnapshotProcessor]
        }.right.get
      }.assembleUTXOState.isLeft shouldBe true
    }

    "check tree assembly(with correct nodes) with default chunk height" in {

      val (_, processor, _, _, manifest, history) = initializeTestState()

      val sn = settings
        .copy(
          levelDB = settings.levelDB.copy(maxVersions = 5),
          snapshotSettings = settings.snapshotSettings.copy(newSnapshotCreationHeight = 10)
        )

      val chunks = manifest
        .chunksKeys
        .flatMap(processor.getChunkById)
        .flatMap(SnapshotChunkSerializer.fromProto(_).toOption)

      val secondProcessor: SnapshotProcessor =
        SnapshotProcessor
          .create(sn, tmpDir)
          .initializeApplicableChunksCache(history, 80).right.get

      chunks.foldLeft(secondProcessor) { case (proc, nextChunk) =>
        val wUpdatedCache = proc.updateCache(nextChunk)
        wUpdatedCache.processNextApplicableChunk(wUpdatedCache).leftFlatMap {
            case e: ApplicableChunkIsAbsent => e.processor.asRight[FastSyncException]
            case t                          => t.asLeft[SnapshotProcessor]
        }.right.get
      }.assembleUTXOState.right.get.tree.selfInspectionAfterFastSync shouldBe true
    }

    "check tree assembly(with incorrect nodes) with default chunk height" in {
      val (_, processor, _, _, manifest, history) = initializeTestState()

      val sn = settings
        .copy(
          levelDB = settings.levelDB.copy(maxVersions = 5),
          snapshotSettings = settings.snapshotSettings.copy(newSnapshotCreationHeight = 10)
        )

      val chunks = manifest
        .chunksKeys
        .flatMap(processor.getChunkById)
        .flatMap(SnapshotChunkSerializer.fromProto(_).toOption)

      val secondProcessor: SnapshotProcessor =
        SnapshotProcessor
          .create(sn, tmpDir)
          .initializeApplicableChunksCache(history, 80).right.get

      chunks.foldLeft(secondProcessor) { case (proc, nextChunk) =>
        val node = nextChunk.node match {
          case l@InternalNode(_, _, _, _, _, _) =>
            l.copy(height = random.nextInt())
          case l => l
        }
        val wUpdatedCache = proc.updateCache(nextChunk.copy(node = node))
        wUpdatedCache.processNextApplicableChunk(wUpdatedCache).leftFlatMap {
          case e: ApplicableChunkIsAbsent => e.processor.asRight[FastSyncException]
          case t                          => t.asLeft[SnapshotProcessor]
        }.right.get
      }.assembleUTXOState.isLeft shouldBe true
    }

  }

}
