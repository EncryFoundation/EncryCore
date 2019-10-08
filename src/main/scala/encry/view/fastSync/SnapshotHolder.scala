package encry.view.fastSync

import NodeMsg.NodeProtoMsg
import SnapshotChunkProto.SnapshotChunkMessage
import SnapshotManifestProto.SnapshotManifestProtoMessage
import akka.actor.{Actor, ActorRef}
import cats.syntax.either._
import com.google.common.primitives.{Ints, Longs}
import com.google.protobuf.ByteString
import com.typesafe.scalalogging.StrictLogging
import encry.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import encry.settings.EncryAppSettings
import encry.storage.VersionalStorage
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.view.fastSync.SnapshotHolder._
import encry.view.fastSync.SnapshotProcessor.{InfoToInsert, InfoToRemove}
import encry.view.state.UtxoState
import encry.view.state.avlTree.utils.implicits.Instances._
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId

class SnapshotHolder(storage: VersionalStorage, settings: EncryAppSettings) extends Actor with StrictLogging with AutoCloseable {

  var snapshotProcessor: SnapshotProcessor = SnapshotProcessor.empty(settings)

  var manifestCollection: Map[Int, List[(SnapshotManifest, List[StorageKey])]] = Map.empty

  var manifestList: List[(SnapshotManifest, List[StorageKey])] = List.empty
  var prevHeight: Int = 0

  // networkController ! RegisterMessagesHandler(smth, self)

  override def receive: Receive = {
    case UpdateSnapshot(block, state) =>
      val (newProcessor: SnapshotProcessor, toInsert: InfoToInsert) =
        snapshotProcessor.processNewSnapshot(state, block)
      snapshotProcessor = newProcessor
      sender() ! toInsert

    case SemanticallySuccessfulModifier(block: Block) =>
      val (newProcessor: SnapshotProcessor, toRemove: InfoToRemove) =
        snapshotProcessor.processNewBlock(block)
      snapshotProcessor = newProcessor
      //stateApplicator() ! toRemove

    case SemanticallySuccessfulModifier(_) =>

    case UpdateSnapshot(block, state) =>
      val snapshot: Option[(SnapshotManifest, List[StorageKey])] = manifestList.find { case (manifest, _) =>
        (manifest.bestBlockId sameElements block.id) && (state.tree.rootHash sameElements manifest.rootHash)
      }
      if (snapshot.isDefined) {
        logger.info(s"Snapshot holder received message UpdateSnapshot with block ${block.encodedId} at height " +
          s"${block.header.height} and root hash ${Algos.encode(state.tree.rootHash)} but current snapshot " +
          s"has already existed.")
        val droppedMList: List[(SnapshotManifest, List[StorageKey])] =
          manifestList.filterNot(e => snapshot.exists(_._1.ManifestId.sameElements(e._1.ManifestId)))
        logger.info(s"Updated manifest list size is ${droppedMList.size}. Prev size is ${manifestList.size}.")
        manifestList = snapshot.fold(droppedMList)(_ :: droppedMList)
      }
      else {
        logger.info(s"Snapshot holder got state at new height ${block.header.height}. Starting processing new state.")
        val (manifest: SnapshotManifest, chunks: List[SnapshotChunk]) = state.tree.initializeSnapshotData(block)
        val snapshotToDB: Seq[(StorageKey, StorageValue)] = chunks.map { elem =>
          val bytes: Array[Byte] = SnapshotChunkSerializer.toProto(elem).toByteArray
          StorageKey @@ Algos.hash(bytes) -> StorageValue @@ bytes
        }
        if (manifestList.headOption.exists(_._1.bestBlockHeight < block.header.height)) {
          logger.info(s"Removed all previous snapshots. Inserted new one.")
          val toDelete: List[StorageKey] = manifestList.flatMap(_._2)
          manifestList = List(manifest -> snapshotToDB.map(_._1).toList)
          storage.insert(
            StorageVersion @@ manifest.ManifestId,
            snapshotToDB.toList,
            toDelete
          )
        } else {
          logger.info(s"Updating snapshot collection.")
          manifestList = (manifest -> snapshotToDB.map(_._1).toList) :: manifestList
          storage.insert(
            StorageVersion @@ manifest.ManifestId,
            snapshotToDB.toList,
            List.empty
          )
        }
      }
    case RequestManifest(remote) =>
      logger.info(s"Got request from $remote.")
    // remote ! ResponseManifest(lastManifest)
    case RequestSnapshot(remote) =>
      logger.info(s"Got request for snapshot from $remote.")
  }

  override def close(): Unit = storage.close()
}

object SnapshotHolder {

  final case class SnapshotManifest(bestBlockId: ModifierId,
                                    rootHash: Array[Byte],
                                    rootNodeBytes: NodeProtoMsg,
                                    stateChunksNumber: Long,
                                    bestBlockHeight: Int) {
    val ManifestId: Array[Byte] = Algos.hash(
      Longs.toByteArray(stateChunksNumber) ++ bestBlockId ++ rootHash ++
        rootNodeBytes.toByteArray ++ Ints.toByteArray(bestBlockHeight)
    )
  }

  object SnapshotManifestSerializer {

    def toProto(manifest: SnapshotManifest): SnapshotManifestProtoMessage = SnapshotManifestProtoMessage()
      .withBestBlockId(ByteString.copyFrom(manifest.bestBlockId))
      .withRootHash(ByteString.copyFrom(manifest.rootHash))
      .withRootNodeBytes(manifest.rootNodeBytes)
      .withStateChunksNumber(manifest.stateChunksNumber)
      .withBestBlockHeight(manifest.bestBlockHeight)


    def fromProto(manifest: SnapshotManifestProtoMessage): Either[Throwable, SnapshotManifest] = Either.catchNonFatal(
      SnapshotManifest(
        ModifierId @@ manifest.bestBlockId.toByteArray,
        manifest.rootHash.toByteArray,
        manifest.rootNodeBytes.get,
        manifest.stateChunksNumber,
        manifest.bestBlockHeight
      )
    )
  }

  final case class SnapshotChunk(nodesList: List[NodeProtoMsg], manifestId: Array[Byte] = Array.emptyByteArray)

  object SnapshotChunkSerializer {

    def toProto(chunk: SnapshotChunk): SnapshotChunkMessage = SnapshotChunkMessage()
      .withChunks(chunk.nodesList)
      .withManifestId(ByteString.copyFrom(chunk.manifestId))

    def fromProto(chunk: SnapshotChunkMessage): Either[Throwable, SnapshotChunk] = Either.catchNonFatal(
      SnapshotChunk(chunk.chunks.toList, chunk.manifestId.toByteArray)
    )
  }

  final case class UpdateSnapshot(bestBlock: Block, state: UtxoState)

  final case class RequestManifest(remote: ActorRef) extends AnyVal

  final case class ResponseManifest(manifest: SnapshotManifest) extends AnyVal

  final case class RequestSnapshot(remote: ActorRef) extends AnyVal

}