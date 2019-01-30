package encry.storage

import com.typesafe.scalalogging.StrictLogging
import encry.utils.CoreTaggedTypes.VersionTag
import io.iohk.iodb.{ByteArrayWrapper, Store}
import org.encryfoundation.common.Algos

import scala.util.{Failure, Success, Try}

trait EncryStorage extends AutoCloseable with StrictLogging {

  val store: Store

  def insert(version: ByteArrayWrapper, toInsert: Seq[(ByteArrayWrapper, ByteArrayWrapper)]): Unit =
    store.update(version, Seq.empty, toInsert)

  def remove(version: ByteArrayWrapper, toRemove: Seq[ByteArrayWrapper]): Unit =
    store.update(version, toRemove, Seq.empty)

  def update(version: ByteArrayWrapper,
             toRemove: Seq[ByteArrayWrapper],
             toUpdate: Seq[(ByteArrayWrapper, ByteArrayWrapper)]): Unit = {
    remove(version, toRemove)
    insert(ByteArrayWrapper(Algos.hash(version.data)), toUpdate)
  }

  def get(key: ByteArrayWrapper): Option[Array[Byte]] = store.get(key).map(_.data)

  def rollbackTo(version: VersionTag): Try[Unit] = store.get(ByteArrayWrapper(version)) match {
      case Some(_) => Success(store.rollback(ByteArrayWrapper(version)))
      case None => Failure(new Exception(s"Unable to get root hash at version ${Algos.encoder.encode(version)}"))
    }

  override def close(): Unit = {
    logger.info("Closing storage")
    store.close()
  }
}

object EncryStorage {

  implicit def liftByteArray(array: Array[Byte]): ByteArrayWrapper = ByteArrayWrapper(array)
}
