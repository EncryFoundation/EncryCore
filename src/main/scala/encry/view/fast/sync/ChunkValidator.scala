package encry.view.fast.sync

import encry.view.fast.sync.SnapshotHolder.SnapshotChunk
import io.iohk.iodb.ByteArrayWrapper
import cats.syntax.either._
import encry.view.fast.sync.FastSyncExceptions.{ChunkValidationError, InconsistentId, NotApplicableId}
import org.encryfoundation.common.utils.Algos
import scala.collection.immutable.HashSet

object ChunkValidator {

//  def checkForApplicableChunk(chunkId: ByteArrayWrapper,
//                              chunksToApply: HashSet[ByteArrayWrapper]): Either[ChunkValidationError, SnapshotChunk] = {
//    if (chunksToApply.contains(chunkId) true.asRight[ChunkValidationError]
//    else
//      NotApplicableId(s"ChunksToApply doesn't contains ${Algos.encode(chunk.node.hash)}")
//      .asLeft[SnapshotChunk]
//  }

  def checkForIdConsistent(chunk: SnapshotChunk): Either[ChunkValidationError, SnapshotChunk] = {
    if (chunk.node.hash sameElements chunk.id) chunk.asRight[ChunkValidationError]
    else InconsistentId(s"Node hash(${Algos.encode(chunk.node.hash)}) " +
      s"doesn't equals to chunk id: ${Algos.encode(chunk.id)}").asLeft[SnapshotChunk]
  }
}
