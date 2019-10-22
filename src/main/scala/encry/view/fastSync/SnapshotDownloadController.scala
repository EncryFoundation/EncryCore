package encry.view.fastSync

import NodeMsg.NodeProtoMsg
import SnapshotChunkProto.SnapshotChunkMessage
import SnapshotManifestProto.SnapshotManifestProtoMessage
import com.typesafe.scalalogging.StrictLogging
import encry.view.fastSync.SnapshotHolder.{ SnapshotChunkSerializer, SnapshotManifestSerializer }
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.settings.EncryAppSettings
import encry.view.history.History
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.network.BasicMessagesRepo.{ NetworkMessage, RequestChunkMessage }
import org.encryfoundation.common.utils.Algos
import cats.syntax.either._
import cats.syntax.option._

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
  ): Either[Throwable, (SnapshotDownloadController, Option[NodeProtoMsg])] = {
    logger.info(s"Got new manifest from ${remote.socketAddress}.")
    val isManifestValid: Boolean = checkManifestValidity(manifestProto.manifestId.toByteArray, history)
    if (isManifestValid && cp.isEmpty) {
      Either.fromTry(SnapshotManifestSerializer.fromProto(manifestProto)) match {
        case Left(error) =>
          logger.info(s"Manifest was parsed with error ${error.getCause}. Ban peer!")
          error.asLeft[(SnapshotDownloadController, Option[NodeProtoMsg])]
        case Right(manifest) =>
          logger.info(s"Manifest ${Algos.encode(manifest.manifestId)} was parsed successfully.")
          (SnapshotDownloadController(
            requiredManifestId,
            manifest.chunksKeys,
            Set.empty[ByteArrayWrapper],
            settings,
            remote.some,
            requiredManifestHeight
          ) -> manifest.rootNodeBytes.some).asRight[Throwable]
      }
    } else if (!isManifestValid) {
      logger.info(s"Got invalid manifest with id ${Algos.encode(manifestProto.manifestId.toByteArray)}. Ban peer!")
      new Exception(s"Got invalid manifest with id ${Algos.encode(manifestProto.manifestId.toByteArray)}")
        .asLeft[(SnapshotDownloadController, Option[NodeProtoMsg])]
    } else {
      logger.info(s"Got manifest message, but we already have another one. Skip this one.")
      (this -> none).asRight[Throwable]
    }
  }

  def processRequestedChunk(
    chunkMessage: SnapshotChunkMessage,
    remote: ConnectedPeer
  ): Either[Throwable, (SnapshotDownloadController, List[NodeProtoMsg])] = {
    logger.info(s"Got new chunk from ${remote.socketAddress}.")
    if (cp.exists(_.socketAddress == remote.socketAddress))
      Either.fromTry(SnapshotChunkSerializer.fromProto(chunkMessage)) match {
        case Left(error) =>
          logger.info(s"Chunk was parsed with error ${error.getCause}. Ban peer!")
          error.asLeft[(SnapshotDownloadController, List[NodeProtoMsg])]
        case Right(chunk) =>
          val chunkId: ByteArrayWrapper = ByteArrayWrapper(chunk.id)
          if (requestedChunks.contains(chunkId)) {
            logger.info(s"Got valid chunk ${Algos.encode(chunk.id)}.")
            (this.copy(requestedChunks = requestedChunks - chunkId, cp = remote.some) -> chunk.nodesList)
              .asRight[Throwable]
          } else {
            logger.info(s"Got unexpected chunk ${Algos.encode(chunk.id)}.")
            new Exception(s"Got unexpected chunk ${Algos.encode(chunk.id)}.")
              .asLeft[(SnapshotDownloadController, List[NodeProtoMsg])]
          }
      } else {
      logger.info(s"Got chunk from unknown peer ${remote.socketAddress}.")
      (this -> List.empty[NodeProtoMsg]).asRight[Throwable]
    }
  }

  def processNextRequestChunksMessage: Either[Boolean, (SnapshotDownloadController, List[NetworkMessage])] = {
    logger.info(s"Current requested queue ${requestedChunks.size}. Will be requested queue ${notYetRequested.size}.")
    if (notYetRequested.nonEmpty) {
      logger.info(s"Not yet requested not empty. Calculated new ids to request")
      chunksIdsToDownload.asRight[Boolean]
    } else if (cp.nonEmpty) {
      logger.info(s"Not yet requested is empty. Fast sync is done.")
      true.asLeft[(SnapshotDownloadController, List[NetworkMessage])]
    } else false.asLeft[(SnapshotDownloadController, List[NetworkMessage])]
  }

  def processManifestHasChangedMessage(
    previousManifestId: Array[Byte],
    newManifest: SnapshotManifestProtoMessage,
    history: History,
    remote: ConnectedPeer
  ): Either[Throwable, (SnapshotDownloadController, Option[NodeProtoMsg])] = {
    logger.info(s"Got message Manifest has changed from ${remote.socketAddress}.")
    val isValidManifest: Boolean = requiredManifestId.sameElements(previousManifestId) &&
      checkManifestValidity(newManifest.manifestId.toByteArray, history)
    if (isValidManifest)
      Either.fromTry(SnapshotManifestSerializer.fromProto(newManifest)) match {
        case Left(error) =>
          logger.info(s"New manifest after manifest has changed was parsed with error ${error.getCause}. Ban peer!")
          error.asLeft[(SnapshotDownloadController, Option[NodeProtoMsg])]
        case Right(newManifest) =>
          logger.info(
            s"Manifest ${Algos.encode(newManifest.manifestId)} after manifest has changed was parsed successfully."
          )
          (SnapshotDownloadController(
            newManifest.manifestId,
            newManifest.chunksKeys,
            Set.empty[ByteArrayWrapper],
            settings,
            remote.some,
            requiredManifestHeight
          ) -> newManifest.rootNodeBytes.some).asRight[Throwable]
      } else {
      logger.info(s"Ban peer for invalid new manifest!")
      new Exception("Invalid manifest has changed message").asLeft[(SnapshotDownloadController, Option[NodeProtoMsg])]
    }
  }

  private def chunksIdsToDownload: (SnapshotDownloadController, List[NetworkMessage]) = {
    val newToRequest: Set[ByteArrayWrapper] = notYetRequested
      .take(settings.snapshotSettings.chunksNumberPerRequestWhileFastSyncMod)
      .map(ByteArrayWrapper(_))
      .toSet
    val updatedToRequest: List[Array[Byte]] =
      notYetRequested.drop(settings.snapshotSettings.chunksNumberPerRequestWhileFastSyncMod)
    val serializedToDownload: List[RequestChunkMessage] = newToRequest
      .map(e => RequestChunkMessage(e.data))
      .toList
    this.copy(notYetRequested = updatedToRequest, requestedChunks = newToRequest) -> serializedToDownload
  }

  private def checkManifestValidity(manifestId: Array[Byte], history: History): Boolean =
    history.getBestHeaderAtHeight { requiredManifestHeight }.exists { header =>
      Algos.hash(header.stateRoot ++ header.id).sameElements(manifestId)
    }
}

object SnapshotDownloadController {
  def empty(settings: EncryAppSettings): SnapshotDownloadController =
    SnapshotDownloadController(Array.emptyByteArray, List.empty, Set.empty, settings, none, 0)
}
