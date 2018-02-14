package encry.view

import io.iohk.iodb.{ByteArrayWrapper, Store}
import scorex.core.ModifierId
import scorex.core.utils.ScorexLogging

trait EncryBaseStorage extends AutoCloseable with ScorexLogging {

  val db: Store

  // TODO: Remove after substitution in history*
  def contains(key: ModifierId): Boolean = db.get(ByteArrayWrapper(key)).isDefined

  // TODO: Remove after substitution in history*
  def update(id: ModifierId,
             idsToRemove: Seq[ByteArrayWrapper],
             toInsert: Seq[(ByteArrayWrapper, ByteArrayWrapper)]): Unit = {
    db.update(
      ByteArrayWrapper(id),
      idsToRemove,
      toInsert)
  }

  def insert(version: Array[Byte],
             toInsert: Seq[(ByteArrayWrapper, ByteArrayWrapper)]): Unit =
    db.update(ByteArrayWrapper(version), Seq.empty, toInsert)

  def get(key: ByteArrayWrapper): Option[Array[Byte]] = db.get(key).map(_.data)

  def getAndUnpackComplexValue(key: ByteArrayWrapper, unitLen: Int): Option[Seq[Array[Byte]]] =
    db.get(key).map { v =>
      v.data.sliding(unitLen, unitLen).foldLeft(Seq[Array[Byte]]())(_ :+ _)
        .ensuring(v.data.length % unitLen == 0, "Value is inconsistent.")
    }

  override def close(): Unit = {
    log.info("Closing history storage...")
    db.close()
  }
}
