package encry.view.fast.sync

import com.typesafe.scalalogging.StrictLogging
import encry.nvg.SnapshotProcessorActor.SnapshotManifest.ChunkId
import encry.settings.EncryAppSettings
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
   * @param groups - elements for insertion into db
   */
  def insertMany(groups: List[List[ChunkId]]): Either[Throwable, Unit] =
    readWrite(
      (batch, _, _) => {
        groups.zipWithIndex.foreach {
          case (group, index) =>
            logger.debug(s"Inserted ${group.size} chunks with group number $index.")
            logger.debug(s"First element of the group is: ${group.headOption.map(Algos.encode)}")
            batch.put(nextGroupKey(index), group.flatten.toArray)
        }
        logger.debug(s"insertMany -> groupsCount -> ${groups.size}.")
      }
    )

  /**
   * @return - returns next chunks ids for request
   */
  def getNextForRequest(groupNumber: Int): Either[Throwable, List[ChunkId]] =
    readWrite(
      (batch, readOptions, _) => {
        logger.debug(s"Going to get next group for request with number $groupNumber.")
        val res = storage.get(nextGroupKey(groupNumber), readOptions)
        val buffer: List[ChunkId] =
          if (res != null) {
            val value = res.grouped(32).toList.map(ChunkId @@ _)
            logger.debug(s"Gotten group is non empty. Elements number is ${value.size}.")
            logger.debug(s"First element of the group is: ${value.headOption.map(Algos.encode)}")
            batch.delete(nextGroupKey(groupNumber))
            value
          } else {
            logger.debug(s"Requested group is null")
            throw new Exception("Inconsistent snapshot download controller db state!")
          }
        buffer
      }
    )
}
