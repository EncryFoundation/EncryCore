package encry.view.fast.sync

import SnapshotChunkProto.SnapshotChunkMessage
import SnapshotManifestProto.SnapshotManifestProtoMessage
import com.typesafe.scalalogging.StrictLogging
import SnapshotHolder.{ SnapshotChunk, SnapshotChunkSerializer, SnapshotManifestSerializer }
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.settings.EncryAppSettings
import encry.view.history.History
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.utils.Algos
import cats.syntax.either._
import cats.syntax.option._
import encry.view.fast.sync.FastSyncExceptions.{ ChunkValidationError, InvalidChunkBytes }
import encry.view.fast.sync.FastSyncExceptions.{
  InvalidManifestBytes,
  ProcessManifestHasChangedMessageException,
  SnapshotDownloadControllerException
}
import org.encryfoundation.common.network.BasicMessagesRepo.{ NetworkMessage, RequestChunkMessage }

final case class SnapshotDownloadController(requiredManifestId: Array[Byte],
                                            notYetRequested: List[Array[Byte]],
                                            requestedChunks: Set[ByteArrayWrapper],
                                            settings: EncryAppSettings,
                                            cp: Option[ConnectedPeer],
                                            requiredManifestHeight: Int)
    extends StrictLogging {

  def processManifest(
    manifestProto: SnapshotManifestProtoMessage,
    remote: ConnectedPeer,
    history: History
  ): Either[SnapshotDownloadControllerException, SnapshotDownloadController] = {
    logger.info(s"Got new manifest from ${remote.socketAddress}.")
    Either.fromTry(SnapshotManifestSerializer.fromProto(manifestProto)) match {
      case Left(error) =>
        logger.info(s"Manifest was parsed with error ${error.getCause}.")
        InvalidManifestBytes(s"Manifest was parsed with error ${error.getCause}")
          .asLeft[SnapshotDownloadController]
      case Right(manifest) =>
        logger.info(s"Manifest ${Algos.encode(manifest.manifestId)} is correct.")
        SnapshotDownloadController(
          requiredManifestId,
          manifest.chunksKeys,
          Set.empty[ByteArrayWrapper],
          settings,
          remote.some,
          requiredManifestHeight
        ).asRight[SnapshotDownloadControllerException]
    }
  }

  def processRequestedChunk(
    chunkMessage: SnapshotChunkMessage,
    remote: ConnectedPeer
  ): Either[ChunkValidationError, (SnapshotDownloadController, SnapshotChunk)] = {
    logger.info(s"Got new chunk from ${remote.socketAddress}.")
    Either.fromTry(SnapshotChunkSerializer.fromProto(chunkMessage)) match {
      case Left(error) =>
        logger.info(s"Chunk was parsed with error ${error.getCause}.")
        InvalidChunkBytes(s"Chunk was parsed with error ${error.getCause}.")
          .asLeft[(SnapshotDownloadController, SnapshotChunk)]
      case Right(chunk) =>
        val chunkId: ByteArrayWrapper = ByteArrayWrapper(chunk.id)
        if (requestedChunks.contains(chunkId)) {
          logger.info(s"Got valid chunk ${Algos.encode(chunk.id)}.")
          (this.copy(requestedChunks = requestedChunks - chunkId), chunk).asRight[InvalidChunkBytes]
        } else {
          logger.info(s"Got unexpected chunk ${Algos.encode(chunk.id)}.")
          InvalidChunkBytes(s"Got unexpected chunk ${Algos.encode(chunk.id)}.")
            .asLeft[(SnapshotDownloadController, SnapshotChunk)]
        }
    }
  }

  def processManifestHasChangedMessage(
    newManifest: SnapshotManifestProtoMessage,
    remote: ConnectedPeer
  ): Either[ProcessManifestHasChangedMessageException, SnapshotDownloadController] = {
    logger.info(
      s"Got message Manifest has changed from ${remote.socketAddress}. " +
        s"New manifest id is ${Algos.encode(newManifest.manifestId.toByteArray)}"
    )
    Either.fromTry(SnapshotManifestSerializer.fromProto(newManifest)) match {
      case Left(error) =>
        ProcessManifestHasChangedMessageException(
          s"New manifest after manifest has changed was parsed with error ${error.getCause}."
        ).asLeft[SnapshotDownloadController]
      case Right(newManifest) =>
        logger.info(s"Manifest ${Algos.encode(newManifest.manifestId)} contains correct root node.")
        SnapshotDownloadController(
          newManifest.manifestId,
          newManifest.chunksKeys,
          Set.empty[ByteArrayWrapper],
          settings,
          remote.some,
          requiredManifestHeight
        ).asRight[ProcessManifestHasChangedMessageException]
    }
  }

  def chunksIdsToDownload: (SnapshotDownloadController, List[NetworkMessage]) = {
    val newToRequest: Set[ByteArrayWrapper] = notYetRequested
      .take(settings.snapshotSettings.chunksNumberPerRequestWhileFastSyncMod)
      .map(ByteArrayWrapper(_))
      .toSet
    val updatedToRequest: List[Array[Byte]] =
      notYetRequested.drop(settings.snapshotSettings.chunksNumberPerRequestWhileFastSyncMod)
    val serializedToDownload: List[RequestChunkMessage] = newToRequest
      .map(id => RequestChunkMessage(id.data))
      .toList
    this.copy(notYetRequested = updatedToRequest, requestedChunks = newToRequest) -> serializedToDownload
  }

  def checkManifestValidity(manifestId: Array[Byte], history: History): Boolean =
    history.getBestHeaderAtHeight { requiredManifestHeight }.exists { header =>
      Algos.hash(header.stateRoot ++ header.id).sameElements(manifestId)
    }

  def canNewManifestBeProcessed: Boolean = cp.isEmpty

  def canChunkBeProcessed(remote: ConnectedPeer): Boolean = cp.exists(_.socketAddress == remote.socketAddress)
}

object SnapshotDownloadController {

  def empty(settings: EncryAppSettings): SnapshotDownloadController =
    SnapshotDownloadController(Array.emptyByteArray, List.empty, Set.empty, settings, none, 0)
}
