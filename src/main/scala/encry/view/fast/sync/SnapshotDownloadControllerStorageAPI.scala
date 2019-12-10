package encry.view.fast.sync

import cats.syntax.either._
import com.typesafe.scalalogging.StrictLogging
import encry.settings.EncryAppSettings
import encry.view.fast.sync.FastSyncExceptions.{FastSyncException, SnapshotDownloadControllerStorageAPIError}
import org.encryfoundation.common.utils.Algos
import org.iq80.leveldb.DB

trait SnapshotDownloadControllerStorageAPI extends DBTryCatchFinallyProvider with StrictLogging {

  val storage: DB

  val settings: EncryAppSettings

  def nextGroupKey(n: Int): Array[Byte] = Algos.hash(s"next_group_key_$n")

  /**
   * Key has 'nextGroupKey' format
   * Value is a batch of elements.
   * Batch is the group of chunks' ids with size 'chunksNumberPerRequestWhileFastSyncMod'.
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
            logger.info(s"Inserted ${group.size} chunks with group number $groupNumber.")
            logger.info(s"First element of the group is: ${group.headOption.map(Algos.encode)}")
            batch.put(nextGroupKey(groupNumber), group.flatten.toArray)
            groupNumber + 1
        }
        logger.info(s"insertMany -> groupsCount -> $groupsCount.")
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
        logger.info(s"Going to get next group for request with number $groupNumber.")
        val res = storage.get(nextGroupKey(groupNumber), readOptions)
        val buffer: List[Array[Byte]] =
          if (res != null) {
            val value = res.grouped(32).toList
            logger.info(s"Gotten group is non empty. Elements number is ${value.size}.")
            logger.info(s"First element of the group is: ${value.headOption.map(Algos.encode)}")
            batch.delete(nextGroupKey(groupNumber))
            value
          } else {
            logger.info(s"Requested group is null")
            List.empty[Array[Byte]]
          }
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
