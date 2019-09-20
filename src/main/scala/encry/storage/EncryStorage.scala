package encry.storage

import com.typesafe.scalalogging.StrictLogging
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.utils.CoreTaggedTypes.VersionTag
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.utils.Algos
import scorex.crypto.hash.Digest32

import scala.util.Try

trait EncryStorage extends AutoCloseable with StrictLogging {

  val store: VersionalStorage

  def insert(version: StorageVersion, toInsert: List[(StorageKey, StorageValue)]): Unit =
    store.insert(version, toInsert)

  def remove(version: StorageVersion, toRemove: List[StorageKey]): Unit =
    store.insert(version, List.empty, toRemove)

  def update(version: StorageVersion,
             toUpdate: List[(StorageKey, StorageValue)]): Unit = {
    insert(StorageVersion @@ Algos.hash(version).untag(Digest32), toUpdate)
  }

  def get(key: StorageKey): Option[Array[Byte]] = store.get(key)

  def rollbackTo(version: VersionTag): Try[Unit] = Try {
    store.rollbackTo(StorageVersion @@ version.untag(VersionTag))
  }

  override def close(): Unit = {
    logger.info("Closing storage")
    store.close()
  }
}

object EncryStorage {

  implicit def liftByteArray(array: Array[Byte]): ByteArrayWrapper = ByteArrayWrapper(array)
}
