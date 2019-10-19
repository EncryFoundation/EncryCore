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
  ProcessNextRequestChunksMessageResult,
  ProcessRequestedChunkResult,
  ProcessRequestedManifestResult
}
import encry.view.history.History
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.network.BasicMessagesRepo.{ NetworkMessage, RequestChunkMessage }
import org.encryfoundation.common.utils.Algos

final case class SnapshotDownloadController(currentManifest: Option[SnapshotManifest],
                                            needToBeRequested: List[Array[Byte]],
                                            awaitingResponse: Set[ByteArrayWrapper],
                                            settings: EncryAppSettings,
                                            cp: Option[ConnectedPeer])
    extends StrictLogging {

  def processManifest(manifestProto: SnapshotManifestProtoMessage,
                      remote: ConnectedPeer,
                      history: History,
                      manifestHasChangedMessage: Boolean = false,
                      previousManifestId: Array[Byte] = Array.emptyByteArray): ProcessRequestedManifestResult = {
    val comparisonStateRootResult: Boolean = history
      .getBestHeaderAtHeight(manifestProto.bestBlockHeight)
      .exists(_.stateRoot.sameElements(manifestProto.rootHash.toByteArray))
    if ((currentManifest.isEmpty || (manifestHasChangedMessage && currentManifest.exists(
          _.ManifestId.sameElements(previousManifestId)
        ))) && comparisonStateRootResult && (cp.isEmpty || cp.exists(
          _.socketAddress == remote.socketAddress
        )))
      SnapshotManifestSerializer
        .fromProto(manifestProto)
        .fold(
          ex => {
            logger.info(s"Manifest from the network is corrupted cause ${ex.getMessage}. Ban remote $remote.")
            ProcessRequestedManifestResult(this, isForBan = true, startRequestChunks = false)
          },
          manifest => {
            logger.info(s"A new snapshot serialized successfully. Number of chunks is ${manifest.chunksKeys.size}.")
            ProcessRequestedManifestResult(
              SnapshotDownloadController(manifest.some,
                                         manifest.chunksKeys,
                                         Set.empty[ByteArrayWrapper],
                                         settings,
                                         remote.some),
              isForBan = false,
              startRequestChunks = true
            )
          }
        )
    else if (!comparisonStateRootResult) {
      logger.info(s"Manifest from ${remote.socketAddress} contains corrupted root hash. Ban it.")
      ProcessRequestedManifestResult(this, isForBan = true, startRequestChunks = false)
    } else ProcessRequestedManifestResult(this, isForBan = false, startRequestChunks = false)
  }

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
            val chunkId: ByteArrayWrapper = ByteArrayWrapper(chunk.id)
            val firstCondition: Boolean   = currentManifest.exists(_.ManifestId.sameElements(chunk.manifestId))
            if (firstCondition && awaitingResponse.contains(chunkId)) {
              logger.info(s"Got correct chunk ${Algos.encode(chunk.id)} from ${remote.socketAddress}.")
              val newController: SnapshotDownloadController =
                this.copy(awaitingResponse = awaitingResponse - chunkId, cp = remote.some)
              ProcessRequestedChunkResult(newController, isForBan = false, chunk.nodesList)
            } else if (!firstCondition) {
              logger.info(s"Got chunk from ${remote.socketAddress} with incorrect manifest id. Ban it.")
              ProcessRequestedChunkResult(this, isForBan = true, List.empty)
            } else {
              logger.info(s"Got chunk which not in awaitingResponse collection. Do nothing.")
              ProcessRequestedChunkResult(this.copy(cp = remote.some), isForBan = false, List.empty)
            }
          }
        )
    else {
      logger.info(s"Got chunk from unknown peer ${remote.socketAddress}.")
      ProcessRequestedChunkResult(this, isForBan = false, List.empty)
    }

  def processNextRequestChunksMessage(): ProcessNextRequestChunksMessageResult =
    if (needToBeRequested.nonEmpty && awaitingResponse.isEmpty) {
      val (processor, toDownload) = chunksIdsToDownload
      ProcessNextRequestChunksMessageResult(processor, isSyncDone = false, toDownload)
    } else if (needToBeRequested.isEmpty && awaitingResponse.isEmpty && currentManifest.nonEmpty)
      ProcessNextRequestChunksMessageResult(this, isSyncDone = true, List.empty)
    else
      ProcessNextRequestChunksMessageResult(this, isSyncDone = false, List.empty)

  private def chunksIdsToDownload: (SnapshotDownloadController, List[NetworkMessage]) = {
    val newToRequest: Set[ByteArrayWrapper] = needToBeRequested
      .take(settings.snapshotSettings.chunksNumberPerRequest)
      .map(ByteArrayWrapper(_))
      .toSet
    val updatedToRequest: List[Array[Byte]] = needToBeRequested.drop(settings.snapshotSettings.chunksNumberPerRequest)
    val serializedToDownload: List[RequestChunkMessage] = newToRequest
      .map(
        e => RequestChunkMessage(e.data, currentManifest.map(_.ManifestId).getOrElse(Array.emptyByteArray))
      )
      .toList
    this.copy(needToBeRequested = updatedToRequest, awaitingResponse = newToRequest) -> serializedToDownload
  }
}

object SnapshotDownloadController {
  def empty(settings: EncryAppSettings): SnapshotDownloadController =
    SnapshotDownloadController(none, List.empty, Set.empty, settings, none)

  final case class ProcessRequestedManifestResult(controller: SnapshotDownloadController,
                                                  isForBan: Boolean,
                                                  startRequestChunks: Boolean)

  final case class ProcessRequestedChunkResult(controller: SnapshotDownloadController,
                                               isForBan: Boolean,
                                               newNodes: List[NodeProtoMsg])

  final case class ProcessNextRequestChunksMessageResult(controller: SnapshotDownloadController,
                                                         isSyncDone: Boolean,
                                                         chunksToRequest: List[NetworkMessage])

}
