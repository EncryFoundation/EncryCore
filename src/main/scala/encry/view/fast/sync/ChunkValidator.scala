package encry.view.fast.sync

import encry.view.fast.sync.SnapshotHolder.SnapshotChunk
import io.iohk.iodb.ByteArrayWrapper
import cats.syntax.either._
import org.encryfoundation.common.utils.Algos
import scala.collection.immutable.HashSet

object ChunkValidator {

  sealed trait ChunkValidationError
  final case class InconsistentId(str: String) extends ChunkValidationError
  final case class NotApplicableId(str: String) extends ChunkValidationError
  final case class InvalidChunkBytes(str: String) extends ChunkValidationError

  def checkForApplicableChunk(chunk: SnapshotChunk,
                              chunksToApply: HashSet[ByteArrayWrapper]): Either[ChunkValidationError, SnapshotChunk] = {
    if (chunksToApply.contains(ByteArrayWrapper(chunk.node.hash))) chunk.asRight[ChunkValidationError]
    else
      NotApplicableId(s"ChunksToApply doesn't contains ${Algos.encode(chunk.node.hash)}")
      .asLeft[SnapshotChunk]
  }

  def checkForIdConsistent(chunk: SnapshotChunk): Either[ChunkValidationError, SnapshotChunk] = {
    if (chunk.node.hash sameElements chunk.id) chunk.asRight[ChunkValidationError]
    else InconsistentId(s"Node hash(${Algos.encode(chunk.node.hash)}) " +
      s"doesn't equals to chunk id: ${Algos.encode(chunk.id)}").asLeft[SnapshotChunk]
  }
}
