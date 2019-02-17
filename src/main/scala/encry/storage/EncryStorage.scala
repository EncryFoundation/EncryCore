package encry.storage

import com.typesafe.scalalogging.StrictLogging
import encry.storage.levelDb.versionalLevelDB.VersionalLevelDBCompanion.{LevelDBVersion, VersionalLevelDbKey, VersionalLevelDbValue}
import encry.storage.levelDb.versionalLevelDB.{LevelDbElem, VersionalLevelDB}
import encry.utils.CoreTaggedTypes.VersionTag
import io.iohk.iodb.{ByteArrayWrapper}
import org.encryfoundation.common.Algos

import scala.util.{Try}

trait EncryStorage extends AutoCloseable with StrictLogging {

  val store: VersionalLevelDB

  def insert(version: ByteArrayWrapper, toInsert: Seq[(ByteArrayWrapper, ByteArrayWrapper)]): Unit =
    store.insert(
      LevelDbElem(
        LevelDBVersion @@ version.data,
        toInsert.map{case (key, value) => VersionalLevelDbKey @@ key.data -> VersionalLevelDbValue @@ value.data}.toList
      )
    )

  def remove(version: ByteArrayWrapper, toRemove: Seq[ByteArrayWrapper]): Unit =
    store.insert(
      LevelDbElem(
        LevelDBVersion @@ version.data,
        List.empty,
        toRemove.map(elem => VersionalLevelDbKey @@ elem.data)
      )
    )

  def update(version: ByteArrayWrapper,
             toRemove: Seq[ByteArrayWrapper],
             toUpdate: Seq[(ByteArrayWrapper, ByteArrayWrapper)]): Unit = {
    remove(version, toRemove)
    insert(ByteArrayWrapper(Algos.hash(version.data)), toUpdate)
  }

  def get(key: ByteArrayWrapper): Option[Array[Byte]] = store.get(VersionalLevelDbKey @@ key.data)

  def rollbackTo(version: VersionTag): Try[Unit] = Try{store.rollbackTo(LevelDBVersion @@ version.untag(VersionTag))}

  override def close(): Unit = {
    logger.info("Closing storage")
    store.close()
  }
}

object EncryStorage {

  implicit def liftByteArray(array: Array[Byte]): ByteArrayWrapper = ByteArrayWrapper(array)
}
