package encry.view.fast.sync

import java.io.File
import java.net.InetSocketAddress

import SnapshotChunkProto.SnapshotChunkMessage
import SnapshotManifestProto.SnapshotManifestProtoMessage
import cats.syntax.either._
import cats.syntax.option._
import com.typesafe.scalalogging.StrictLogging
import encry.nvg.fast.sync.SnapshotProcessor.SnapshotManifest.ChunkId
import encry.nvg.fast.sync.SnapshotProcessor.{ SnapshotChunk, SnapshotChunkSerializer, SnapshotManifestSerializer }
import encry.settings.EncryAppSettings
import encry.storage.levelDb.versionalLevelDB.LevelDbFactory
import encry.view.fast.sync.FastSyncExceptions._
import encry.view.history.History
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.network.BasicMessagesRepo.RequestChunkMessage
import org.encryfoundation.common.utils.Algos
import org.iq80.leveldb.{ DB, Options }

final case class SnapshotDownloadController(
  requiredManifestId: Array[Byte],
  awaitedChunks: Set[ByteArrayWrapper],
  settings: EncryAppSettings,
  cp: Option[InetSocketAddress],
  requiredManifestHeight: Int,
  storage: DB,
  batchesSize: Int,
  nextGroupForRequestNumber: Int
) extends SnapshotDownloadControllerStorageAPI
    with StrictLogging
    with AutoCloseable {

  def processManifest(
    manifestProto: SnapshotManifestProtoMessage,
    remote: InetSocketAddress,
    history: History
  ): Either[SnapshotDownloadControllerException, SnapshotDownloadController] = {
    logger.info(s"Got new manifest from $remote.")
    Either.fromTry(SnapshotManifestSerializer.fromProto(manifestProto)) match {
      case Left(error) =>
        logger.info(s"Manifest was parsed with error ${error.getCause}.")
        InvalidManifestBytes(s"Manifest was parsed with error ${error.getCause}")
          .asLeft[SnapshotDownloadController]
      case Right(manifest) =>
        logger.info(s"Manifest ${Algos.encode(manifest.manifestId)} is correct.")
        val groups: List[List[ChunkId]] =
          manifest.chunksKeys.grouped(settings.snapshotSettings.chunksNumberPerRequestWhileFastSyncMod).toList
        val batchesSize: Int = insertMany(groups) match {
          case Left(error) =>
            logger.info(s"Error ${error.getCause} has occurred while processing new manifest.")
            throw new Exception(s"Error ${error.getCause} has occurred while processing new manifest.")
          case Right(_) =>
            val batchesCount: Int = groups.size
            logger.info(s"New chunks ids successfully inserted into db. Number of batches is $batchesCount.")
            batchesCount
        }
        SnapshotDownloadController(
          requiredManifestId,
          Set.empty[ByteArrayWrapper],
          settings,
          remote.some,
          requiredManifestHeight,
          storage,
          batchesSize,
          0
        ).asRight[SnapshotDownloadControllerException]
    }
  }

  def processRequestedChunk(
    chunkMessage: SnapshotChunkMessage,
    remote: InetSocketAddress
  ): Either[ChunkValidationError, (SnapshotDownloadController, SnapshotChunk)] = {
    logger.debug(s"Got new chunk from $remote.")
    Either.fromTry(SnapshotChunkSerializer.fromProto(chunkMessage)) match {
      case Left(error) =>
        logger.info(s"Chunk was parsed with error ${error.getCause}.")
        InvalidChunkBytes(s"Chunk was parsed with error ${error.getCause}.")
          .asLeft[(SnapshotDownloadController, SnapshotChunk)]
      case Right(chunk) =>
        val chunkId: ByteArrayWrapper = ByteArrayWrapper(chunk.id)
        if (awaitedChunks.contains(chunkId)) {
          logger.info(s"Got valid chunk ${Algos.encode(chunk.id)}.")
          (this.copy(awaitedChunks = awaitedChunks - chunkId), chunk).asRight[InvalidChunkBytes]
        } else {
          logger.info(s"Got unexpected chunk ${Algos.encode(chunk.id)}.")
          UnexpectedChunkMessage(s"Got unexpected chunk ${Algos.encode(chunk.id)}.")
            .asLeft[(SnapshotDownloadController, SnapshotChunk)]
        }
    }
  }

  def getNextBatchAndRemoveItFromController
    : Either[FastSyncExceptions.FastSyncException, (SnapshotDownloadController, List[RequestChunkMessage])] =
    (for {
      nextBatch                                       <- getNextForRequest(nextGroupForRequestNumber)
      serializedToDownload: List[RequestChunkMessage] = nextBatch.map(id => RequestChunkMessage(id))
    } yield {
      val newGroupNumber: Int   = nextGroupForRequestNumber + 1
      val lastsBatchesSize: Int = batchesSize - 1
      logger.info(
        s"Successfully got group with number $nextGroupForRequestNumber." +
          s" New group number is $newGroupNumber. Lasts batches size is $lastsBatchesSize"
      )
      this.copy(
        awaitedChunks = nextBatch.map(ByteArrayWrapper(_)).toSet,
        batchesSize = lastsBatchesSize,
        nextGroupForRequestNumber = newGroupNumber
      ) -> serializedToDownload
    }).leftMap(l => ChunksIdsToDownloadException(l.getMessage))

  def isBatchesSizeEmpty: Boolean = batchesSize == 0

  def checkManifestValidity(manifestId: Array[Byte], history: History): Boolean =
    history.getBestHeaderAtHeight { requiredManifestHeight }.exists { header =>
      Algos.hash(header.stateRoot ++ header.id).sameElements(manifestId)
    }

  def canNewManifestBeProcessed: Boolean = cp.isEmpty

  def canChunkBeProcessed(remote: InetSocketAddress): Boolean = cp.contains(remote)

  def reInitFastSync: SnapshotDownloadController =
    try {
      storage.close()
      val dir: File = SnapshotDownloadController.getChunksStorageDir(settings)
      import org.apache.commons.io.FileUtils
      FileUtils.deleteDirectory(dir)
      SnapshotDownloadController
        .empty(settings)
        .copy(
          requiredManifestHeight = this.requiredManifestHeight,
          requiredManifestId = this.requiredManifestId
        )
    } catch {
      case err: Throwable =>
        logger.info(s"Error ${err.getMessage} has occurred")
        throw new Exception(s"Error ${err.getMessage} has occurred")
    }

  def close(): Unit = storage.close()
}

object SnapshotDownloadController {

  def getChunksStorageDir(settings: EncryAppSettings): File = {
    val dir: File = new File(s"${settings.directory}/chunksStorage")
    dir.mkdirs()
    dir
  }

  def empty(settings: EncryAppSettings): SnapshotDownloadController = {
    val levelDBInit = LevelDbFactory.factory.open(getChunksStorageDir(settings), new Options)
    SnapshotDownloadController(Array.emptyByteArray, Set.empty, settings, none, 0, levelDBInit, 0, 0)
  }
}
