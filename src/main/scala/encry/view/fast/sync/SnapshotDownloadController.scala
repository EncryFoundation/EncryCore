package encry.view.fast.sync

import NodeMsg.NodeProtoMsg
import SnapshotChunkProto.SnapshotChunkMessage
import SnapshotManifestProto.SnapshotManifestProtoMessage
import com.typesafe.scalalogging.StrictLogging
import SnapshotHolder.{ SnapshotChunkSerializer, SnapshotManifest, SnapshotManifestSerializer }
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.settings.EncryAppSettings
import encry.view.history.History
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.network.BasicMessagesRepo.{ NetworkMessage, RequestChunkMessage }
import org.encryfoundation.common.utils.Algos
import cats.syntax.either._
import cats.syntax.option._
import encry.storage.VersionalStorage.{ StorageKey, StorageValue }
import encry.view.fast.sync.SnapshotDownloadController.{
  ProcessManifestException,
  ProcessManifestHasChangedMessageException,
  ProcessRequestedChunkException
}
import encry.view.state.avlTree.NodeSerilalizer
import encry.view.state.avlTree.utils.implicits.Instances._
import org.encryfoundation.common.modifiers.history.Header

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
  ): Either[ProcessManifestException, (SnapshotDownloadController, Option[NodeProtoMsg])] = {
    logger.info(s"Got new manifest from ${remote.socketAddress}.")
    val (isManifestValid: Boolean, header: Option[Header]) =
      checkManifestValidity(manifestProto.manifestId.toByteArray, history)
    if (isManifestValid && cp.isEmpty) {
      Either.fromTry(SnapshotManifestSerializer.fromProto(manifestProto)) match {
        case Left(error) =>
          ProcessManifestException(s"Manifest was parsed with error ${error.getCause}")
            .asLeft[(SnapshotDownloadController, Option[NodeProtoMsg])]
        case Right(manifest) if isCorrectRootNode(manifest, header) =>
          logger.info(s"Manifest ${Algos.encode(manifest.manifestId)} contains correct root node.")
          (SnapshotDownloadController(
            requiredManifestId,
            manifest.chunksKeys,
            Set.empty[ByteArrayWrapper],
            settings,
            remote.some,
            requiredManifestHeight
          ) -> manifest.rootNodeBytes.some).asRight[ProcessManifestException]
        case Right(manifest) =>
          ProcessManifestException(s"Manifest ${Algos.encode(manifest.manifestId)} root node is incorrect.")
            .asLeft[(SnapshotDownloadController, Option[NodeProtoMsg])]
      }
    } else if (!isManifestValid) {
      ProcessManifestException(s"Got invalid manifest with id ${Algos.encode(manifestProto.manifestId.toByteArray)}")
        .asLeft[(SnapshotDownloadController, Option[NodeProtoMsg])]
    } else {
      logger.info(s"Got manifest message, but we already have another one. Skip this one.")
      (this -> none).asRight[ProcessManifestException]
    }
  }

  def processRequestedChunk(
    chunkMessage: SnapshotChunkMessage,
    remote: ConnectedPeer
  ): Either[ProcessRequestedChunkException, (SnapshotDownloadController, List[NodeProtoMsg])] = {
    logger.info(s"Got new chunk from ${remote.socketAddress}.")
    if (cp.exists(_.socketAddress == remote.socketAddress))
      Either.fromTry(SnapshotChunkSerializer.fromProto(chunkMessage)) match {
        case Left(error) =>
          ProcessRequestedChunkException(s"Chunk was parsed with error ${error.getCause}.")
            .asLeft[(SnapshotDownloadController, List[NodeProtoMsg])]
        case Right(chunk) =>
          val chunkId: ByteArrayWrapper = ByteArrayWrapper(chunk.id)
          if (requestedChunks.contains(chunkId)) {
            logger.info(s"Got valid chunk ${Algos.encode(chunk.id)}.")
            (this.copy(requestedChunks = requestedChunks - chunkId) -> chunk.nodesList)
              .asRight[ProcessRequestedChunkException]
          } else
            ProcessRequestedChunkException(s"Got unexpected chunk ${Algos.encode(chunk.id)}.")
              .asLeft[(SnapshotDownloadController, List[NodeProtoMsg])]
      } else {
      logger.info(s"Got chunk from unknown peer ${remote.socketAddress}.")
      (this -> List.empty[NodeProtoMsg]).asRight[ProcessRequestedChunkException]
    }
  }

  def processRequestChunksMessage: Either[Boolean, (SnapshotDownloadController, List[NetworkMessage])] = {
    logger.info(s"Current requested queue ${requestedChunks.size}. Will be requested queue ${notYetRequested.size}.")
    if (notYetRequested.nonEmpty) {
      logger.info(s"Not yet requested not empty. Calculated new ids to request")
      chunksIdsToDownload.asRight[Boolean]
    } else {
      logger.info(s"Not yet requested is empty. Fast sync is done.")
      true.asLeft[(SnapshotDownloadController, List[NetworkMessage])]
    }
  }

  def processManifestHasChangedMessage(
    previousManifestId: Array[Byte],
    newManifest: SnapshotManifestProtoMessage,
    history: History,
    remote: ConnectedPeer
  ): Either[ProcessManifestHasChangedMessageException, (SnapshotDownloadController, Option[NodeProtoMsg])] = {
    logger.info(
      s"Got message Manifest has changed from ${remote.socketAddress}. " +
        s"New manifest id is ${Algos.encode(newManifest.manifestId.toByteArray)}"
    )
    val (isValidNewManifest: Boolean, header: Option[Header]) =
      checkManifestValidity(newManifest.manifestId.toByteArray, history)
    val isValidMessage: Boolean = requiredManifestId.sameElements(previousManifestId)
    if (isValidMessage && isValidNewManifest) {
      Either.fromTry(SnapshotManifestSerializer.fromProto(newManifest)) match {
        case Left(error) =>
          ProcessManifestHasChangedMessageException(
            s"New manifest after manifest has changed was parsed with error ${error.getCause}."
          ).asLeft[(SnapshotDownloadController, Option[NodeProtoMsg])]
        case Right(newManifest) if isCorrectRootNode(newManifest, header) =>
          logger.info(s"Manifest ${Algos.encode(newManifest.manifestId)} contains correct root node.")
          (SnapshotDownloadController(
            newManifest.manifestId,
            newManifest.chunksKeys,
            Set.empty[ByteArrayWrapper],
            settings,
            remote.some,
            requiredManifestHeight
          ) -> newManifest.rootNodeBytes.some).asRight[ProcessManifestHasChangedMessageException]
      }
    } else
      ProcessManifestHasChangedMessageException("Invalid manifest has changed message")
        .asLeft[(SnapshotDownloadController, Option[NodeProtoMsg])]
  }

  private def chunksIdsToDownload: (SnapshotDownloadController, List[NetworkMessage]) = {
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

  private def checkManifestValidity(manifestId: Array[Byte], history: History): (Boolean, Option[Header]) = {
    val header: Option[Header] = history.getBestHeaderAtHeight { requiredManifestHeight }
    header.exists { header =>
      Algos.hash(header.stateRoot ++ header.id).sameElements(manifestId)
    } -> header
  }

  private def isCorrectRootNode(manifest: SnapshotManifest, header: Option[Header]): Boolean =
    header.exists { h =>
      NodeSerilalizer
        .fromProto[StorageKey, StorageValue](manifest.rootNodeBytes)
        .hash
        .sameElements(h.stateRoot)
    }
}

object SnapshotDownloadController {

  sealed trait SnapshotDownloadControllerException
  final case class ProcessManifestException(msg: String)                  extends SnapshotDownloadControllerException
  final case class ProcessRequestedChunkException(msg: String)            extends SnapshotDownloadControllerException
  final case class ProcessManifestHasChangedMessageException(msg: String) extends SnapshotDownloadControllerException

  def empty(settings: EncryAppSettings): SnapshotDownloadController =
    SnapshotDownloadController(Array.emptyByteArray, List.empty, Set.empty, settings, none, 0)
}
