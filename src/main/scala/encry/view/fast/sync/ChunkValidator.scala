package encry.view.fast.sync

import encry.view.fast.sync.SnapshotHolder.SnapshotChunk
import io.iohk.iodb.ByteArrayWrapper
import cats.syntax.either._
import encry.view.fast.sync.FastSyncExceptions.{ChunkValidationError, InconsistentChunkId, NotApplicableId}
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


}
