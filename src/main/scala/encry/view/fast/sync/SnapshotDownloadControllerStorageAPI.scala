package encry.view.fast.sync

import cats.syntax.either._
import encry.settings.EncryAppSettings
import encry.view.fast.sync.FastSyncExceptions.{ FastSyncException, SnapshotDownloadControllerStorageAPIError }
import org.encryfoundation.common.utils.Algos
import org.iq80.leveldb.DB

trait SnapshotDownloadControllerStorageAPI extends DBTryCatchFinallyProvider {

  val storage: DB

  val settings: EncryAppSettings

  def nextGroupKey(n: Int): Array[Byte] = Algos.hash(s"next_group_key_$n")

  /**
   * Key is the first element of batch.
   * Value is a batch.
   * Batch is the group of chunks ids with size 'chunksNumberPerRequestWhileFastSyncMod'.
   *
   * @param ids - elements for insertion into db
   */
  def insertMany(ids: List[Array[Byte]]): Either[FastSyncException, Int] =
    readWrite[Either[FastSyncException, Int]](
      (batch, _, _) => {
        val groups: List[List[Array[Byte]]] =
          ids.grouped(settings.snapshotSettings.chunksNumberPerRequestWhileFastSyncMod).toList
        val groupsCount: Int = groups.foldLeft(1) {
          case (groupNumber, group) =>
            batch.put(nextGroupKey(groupNumber), group.flatten.toArray)
            groupNumber + 1
        }
        groupsCount.asRight[FastSyncException]
      },
      err => SnapshotDownloadControllerStorageAPIError(err.getMessage).asLeft[Int]
    )

  /**
   * @return - returns next chunks ids for request
   */
  def getNextForRequest(groupNumber: Int): Either[FastSyncException, List[Array[Byte]]] =
    readWrite[Either[FastSyncException, List[Array[Byte]]]](
      (batch, readOptions, _) => {
        val res = storage.get(nextGroupKey(groupNumber), readOptions)
        val buffer: List[Array[Byte]] =
          if (res != null) {
            batch.delete(nextGroupKey(groupNumber))
            storage.write(batch)
            res.grouped(32).toList
          } else List.empty[Array[Byte]]
        buffer.asRight[FastSyncException]
      },
      err => SnapshotDownloadControllerStorageAPIError(err.getMessage).asLeft[List[Array[Byte]]]
    )

  /**
   * Check if chunk's size 0 or not
   *
   * @return true if current batches size > 0, otherwise false
   */
  def isBatchesListNonEmpty: Either[FastSyncException, Boolean] =
    readWrite[Either[FastSyncException, Boolean]](
      (_, _, iterator) => {
        iterator.seekToFirst()
        iterator.hasNext.asRight[FastSyncException]
      },
      err => SnapshotDownloadControllerStorageAPIError(err.getMessage).asLeft[Boolean]
    )
}
