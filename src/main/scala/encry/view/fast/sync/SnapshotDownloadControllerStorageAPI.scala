package encry.view.fast.sync

import cats.syntax.either._
import com.typesafe.scalalogging.StrictLogging
import encry.settings.EncryAppSettings
import encry.view.fast.sync.FastSyncExceptions.{
  FastSyncException,
  SnapshotDownloadControllerStorageAPIGetManyFunctionFailed,
  SnapshotDownloadControllerStorageAPIInsertMany,
  SnapshotDownloadControllerStorageAPIIsBatchesListNonEmpty
}
import org.encryfoundation.common.utils.Algos
import org.iq80.leveldb.{ DB, DBIterator, ReadOptions, WriteBatch }

trait SnapshotDownloadControllerStorageAPI extends StrictLogging {

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
  def insertMany(ids: List[Array[Byte]]): Either[FastSyncException, Int] = {
    val batch: WriteBatch = storage.createWriteBatch()
    try {
      val groups: List[List[Array[Byte]]] =
        ids.grouped(settings.snapshotSettings.chunksNumberPerRequestWhileFastSyncMod).toList
      val groupsCount: Int = groups.foldLeft(1) {
        case (groupNumber, group) =>
          batch.put(nextGroupKey(groupNumber), group.flatten.toArray)
          groupNumber + 1
      }
      storage.write(batch)
      groupsCount.asRight[FastSyncException]
    } catch {
      case err: Throwable =>
        println(s"While insertMany function error ${err.getMessage} has occurred.")
        SnapshotDownloadControllerStorageAPIInsertMany(err.getMessage).asLeft[Int]
    } finally {
      batch.close()
    }
  }

  /**
   * @return - returns next chunks ids for request
   */
  def getNextForRequest(groupNumber: Int): Either[FastSyncException, List[Array[Byte]]] = {
    val snapshot          = storage.getSnapshot
    val readOptions       = new ReadOptions().snapshot(snapshot)
    val batch: WriteBatch = storage.createWriteBatch()
    try {
      val res = storage.get(nextGroupKey(groupNumber))
      val buffer: List[Array[Byte]] =
        if (res != null) {
          batch.delete(nextGroupKey(groupNumber))
          storage.write(batch)
          res.grouped(32).toList
        } else List.empty[Array[Byte]]
      buffer.asRight[FastSyncException]
    } catch {
      case err: Throwable =>
        println(s"While getMany function error ${err.getMessage} has occurred")
        SnapshotDownloadControllerStorageAPIGetManyFunctionFailed(err.getMessage).asLeft[List[Array[Byte]]]
    } finally {
      batch.close()
      readOptions.snapshot().close()
    }
  }

  /**
   * Check if chunk's size 0 or not
   *
   * @return true if current batches size > 0, otherwise false
   */
  def isBatchesListNonEmpty: Either[FastSyncException, Boolean] = {
    val readOptions = new ReadOptions()
    val snapshot    = storage.getSnapshot
    readOptions.snapshot(snapshot)
    val iterator: DBIterator = storage.iterator(readOptions)
    try {
      iterator.seekToFirst()
      iterator.hasNext.asRight[FastSyncException]
    } catch {
      case err: Throwable =>
        logger.info(s"Error has occurred while isBatchesListNonEmpty function processing.")
        SnapshotDownloadControllerStorageAPIIsBatchesListNonEmpty(err.getMessage).asLeft[Boolean]
    } finally {
      iterator.close()
      readOptions.snapshot().close()
    }
  }
}
