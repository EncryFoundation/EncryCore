package encry.view.fast.sync

object FastSyncExceptions {

  sealed trait FastSyncException {
    val error: String
  }

  sealed trait SnapshotProcessorError extends FastSyncException
  final case class ProcessNewSnapshotError(error: String) extends SnapshotProcessorError
  final case class ProcessNewBlockError(error: String)    extends SnapshotProcessorError

  final case class ChunkApplyError(error: String) extends FastSyncException

  sealed trait UtxoCreationError extends FastSyncException
  final case class EmptyRootNodeError(error: String) extends UtxoCreationError
  final case class EmptyHeightKey(error: String)     extends UtxoCreationError

  sealed trait ChunkValidationError extends FastSyncException
  final case class InconsistentChunkId(error: String) extends ChunkValidationError
  final case class InvalidChunkBytes(error: String) extends ChunkValidationError
  final case class UnexpectedChunkMessage(error: String) extends ChunkValidationError

  sealed trait SnapshotDownloadControllerException extends FastSyncException
  final case class InvalidManifestBytes(error: String)             extends SnapshotDownloadControllerException

  final case class ApplicableChunkIsAbsent(error: String, processor: SnapshotHolder) extends FastSyncException
  final case class BestHeaderAtHeightIsAbsent(error: String) extends FastSyncException
  final case class InitializeHeightAndRootKeysException(error: String) extends FastSyncException
  final case class ChunksIdsToDownloadException(error: String) extends FastSyncException
}