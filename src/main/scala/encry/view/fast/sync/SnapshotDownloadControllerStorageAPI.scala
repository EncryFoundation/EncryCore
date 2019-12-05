package encry.view.fast.sync

import cats.syntax.either._
import com.typesafe.scalalogging.StrictLogging
import encry.settings.EncryAppSettings
import encry.view.fast.sync.FastSyncExceptions.{
  FastSyncException,
  SnapshotDownloadControllerStorageAPIGetBatchesSize,
  SnapshotDownloadControllerStorageAPIGetManyFunctionFailed,
  SnapshotDownloadControllerStorageAPIInsertMany,
  SnapshotDownloadControllerStorageAPIIsBatchesListNonEmpty
}
import org.encryfoundation.common.utils.Algos
import org.iq80.leveldb.{ DB, DBIterator, ReadOptions, WriteBatch }
import com.google.common.primitives.Ints

trait SnapshotDownloadControllerStorageAPI extends StrictLogging {

  val storage: DB

  val settings: EncryAppSettings

  val notRequestedBatchesSizeKey: Array[Byte] = Algos.hash("not_requested_batches_size_key")

  /**
   * Key is the first element of batch.
   * Value is a batch.
   * Batch is the group of chunks ids with size 'chunksNumberPerRequestWhileFastSyncMod'.
   *
   * @param ids - elements for insertion into db
   */
  def insertMany(ids: List[Array[Byte]]): Either[FastSyncException, Unit] = {
    val batch: WriteBatch = storage.createWriteBatch()
    try {
      val groups = ids.grouped(settings.snapshotSettings.chunksNumberPerRequestWhileFastSyncMod)
      groups.foreach { nextBatch =>
        batch.put(nextBatch.head, nextBatch.flatten.toArray)
      }
      batch.put(notRequestedBatchesSizeKey, Ints.toByteArray(groups.size))
      storage.write(batch)
      ().asRight[FastSyncException]
    } catch {
      case err: Throwable =>
        logger.info(s"While insertMany function error ${err.getMessage} has occurred.")
        SnapshotDownloadControllerStorageAPIInsertMany(err.getMessage).asLeft[Unit]
    } finally {
      batch.close()
    }
  }

  /**
   * @return - returns next chunks ids for request
   */
  def getNextForRequest: Either[FastSyncException, List[Array[Byte]]] = {
    val snapshot             = storage.getSnapshot
    val readOptions          = new ReadOptions().snapshot(snapshot)
    val iterator: DBIterator = storage.iterator(readOptions)
    val batch: WriteBatch    = storage.createWriteBatch()
    try {
      iterator.seekToFirst()
      val buffer: List[Array[Byte]] =
        if (iterator.hasNext) {
          val res = iterator.next().getValue.grouped(32).toList
          batch.delete(iterator.next().getKey)
          val currentNonRequested: Int = Ints.fromByteArray(storage.get(notRequestedBatchesSizeKey))
          if (currentNonRequested != 0) {
            batch.put(notRequestedBatchesSizeKey, Ints.toByteArray(currentNonRequested - 1))
          }
          storage.write(batch)
          res
        } else List.empty[Array[Byte]]
      buffer.asRight[FastSyncException]
    } catch {
      case err: Throwable =>
        logger.info(s"While getMany function error ${err.getMessage} has occurred")
        SnapshotDownloadControllerStorageAPIGetManyFunctionFailed(err.getMessage).asLeft[List[Array[Byte]]]
    } finally {
      batch.close()
      iterator.close()
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

  def currentNonRequestedBatchesSize: Either[FastSyncException, Int] =
    try {
      Ints.fromByteArray(storage.get(notRequestedBatchesSizeKey)).asRight[FastSyncException]
    } catch {
      case err: Throwable =>
        logger.info(s"Error has occurred while isBatchesListNonEmpty function processing.")
        SnapshotDownloadControllerStorageAPIGetBatchesSize(err.getMessage).asLeft[Int]
    }
}
