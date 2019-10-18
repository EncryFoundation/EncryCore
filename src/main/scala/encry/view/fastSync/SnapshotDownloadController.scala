package encry.view.fastSync

import NodeMsg.NodeProtoMsg
import SnapshotChunkProto.SnapshotChunkMessage
import SnapshotManifestProto.SnapshotManifestProtoMessage
import com.typesafe.scalalogging.StrictLogging
import encry.view.fastSync.SnapshotHolder.{ SnapshotChunkSerializer, SnapshotManifest, SnapshotManifestSerializer }
import cats.syntax.option._
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.settings.EncryAppSettings
import encry.view.fastSync.SnapshotDownloadController.{
  ProcessManifestHasChangedMessage,
  ProcessRequestedChunkResult,
  ProcessRequestedManifestResult
}
import org.encryfoundation.common.network.BasicMessagesRepo.{ NetworkMessage, RequestChunkMessage }
import org.encryfoundation.common.utils.Algos

final case class SnapshotDownloadController(currentManifest: Option[SnapshotManifest],
                                            toRequest: List[Array[Byte]],
                                            inAwait: List[Array[Byte]],
                                            settings: EncryAppSettings,
                                            cp: Option[ConnectedPeer])
    extends StrictLogging {

  //todo check in history root hash from received manifest
  def processRequestedManifest(manifestProto: SnapshotManifestProtoMessage,
                               remote: ConnectedPeer): ProcessRequestedManifestResult =
    if (currentManifest.isEmpty)
      SnapshotManifestSerializer
        .fromProto(manifestProto)
        .fold(
          ex => {
            logger.info(s"Manifest from network is corrupted cause ${ex.getMessage}.")
            ProcessRequestedManifestResult(this.copy(cp = remote.some), isForBan = true, startRequestChunks = false)
          },
          manifest => {
            logger.info(
              s"New snapshot has successfully serialized. Starting processing chunks. Number of chunks is ${manifest.chunksKeys.size}."
            )
            ProcessRequestedManifestResult(
              SnapshotDownloadController(manifest.some,
                                         manifest.chunksKeys,
                                         List.empty[Array[Byte]],
                                         settings,
                                         remote.some),
              isForBan = false,
              startRequestChunks = true
            )
          }
        )
    else ProcessRequestedManifestResult(this.copy(cp = remote.some), isForBan = false, startRequestChunks = false)

  def processRequestedChunk(chunkMessage: SnapshotChunkMessage, remote: ConnectedPeer): ProcessRequestedChunkResult =
    if (cp.exists(_.socketAddress == remote.socketAddress))
      SnapshotChunkSerializer
        .fromProto(chunkMessage)
        .fold(
          e => {
            logger.info(s"Chunks from ${remote.socketAddress} is corrupted cause ${e.getCause}.")
            ProcessRequestedChunkResult(this.copy(cp = remote.some), isForBan = true, List.empty)
          },
          chunk => {
            if (currentManifest.exists(_.ManifestId.sameElements(chunk.manifestId)) &&
                inAwait.exists(_.sameElements(chunk.id))) {
              val newController = this.copy(inAwait = inAwait.filterNot(_.sameElements(chunk.id)), cp = remote.some)
              ProcessRequestedChunkResult(newController, isForBan = false, chunk.nodesList)
            } else
              ProcessRequestedChunkResult(this.copy(cp = remote.some), isForBan = false, List.empty) //todo probably forBan = true
          }
        )
    else {
      logger.info(s"processRequestedChunk --->>> ELSE for ${remote.socketAddress}")
      ProcessRequestedChunkResult(this, isForBan = false, List.empty)
    }

  def chunksIdsToDownload: (SnapshotDownloadController, List[NetworkMessage]) =
    if (inAwait.isEmpty) {
      val newToRequest: List[Array[Byte]] = toRequest.take(settings.snapshotSettings.chunksNumberPerRequest)
      logger.info(s"newToRequest -> ${newToRequest.map(Algos.encode).mkString(",")}")
      val updatedToRequest: List[Array[Byte]] = toRequest.drop(settings.snapshotSettings.chunksNumberPerRequest)
      //logger.info(s"updatedToRequest -> ${updatedToRequest.map(Algos.encode).mkString(",")}")
      val serializedToDownload: List[RequestChunkMessage] = newToRequest.map(
        e => RequestChunkMessage(e, currentManifest.map(_.ManifestId).getOrElse(Array.emptyByteArray))
      )
      logger.info(s"serializedToDownload -> ${serializedToDownload.map(f => Algos.encode(f.chunkId)).mkString(",")}")
      this.copy(toRequest = updatedToRequest, inAwait = newToRequest) -> serializedToDownload
    } else this -> List.empty

  //todo check in history root hash from received manifest
  def processManifestHasChangedMessage(newManifest: SnapshotManifestProtoMessage,
                                       previousManifest: Array[Byte],
                                       remote: ConnectedPeer): ProcessManifestHasChangedMessage =
    SnapshotManifestSerializer
      .fromProto(newManifest)
      .fold(
        e => {
          logger.info(s"Got message ManifestHasChanged with corrupted new manifest cause ${e.getCause}.")
          ProcessManifestHasChangedMessage(this, isForBan = true)
        },
        manifest => {
          if (currentManifest.exists(_.ManifestId.sameElements(previousManifest)) && cp.exists(
                _.socketAddress == remote.socketAddress
              )) {
            ProcessManifestHasChangedMessage(
              SnapshotDownloadController(manifest.some, manifest.chunksKeys, List.empty, settings, remote.some),
              isForBan = false
            )
          } else ProcessManifestHasChangedMessage(this.copy(cp = remote.some), isForBan = true)
        }
      )
}

object SnapshotDownloadController {
  def empty(settings: EncryAppSettings): SnapshotDownloadController =
    SnapshotDownloadController(none, List.empty, List.empty, settings, none)

  final case class ProcessRequestedManifestResult(controller: SnapshotDownloadController,
                                                  isForBan: Boolean,
                                                  startRequestChunks: Boolean)

  final case class ProcessRequestedChunkResult(controller: SnapshotDownloadController,
                                               isForBan: Boolean,
                                               newNodes: List[NodeProtoMsg])

  final case class ProcessManifestHasChangedMessage(controller: SnapshotDownloadController, isForBan: Boolean)

}
