package encry.view

import encry.settings.Algos
import io.iohk.iodb.{ByteArrayWrapper, Store}
import scorex.core.ModifierId
import scorex.core.utils.ScorexLogging

trait EncryBaseStorage extends AutoCloseable with ScorexLogging {

  val db: Store

  def contains(id: ModifierId): Boolean = db.get(ByteArrayWrapper(id)).isDefined

  def insert(id: ModifierId, toInsert: Seq[(ByteArrayWrapper, ByteArrayWrapper)]): Unit = update(id, Seq(), toInsert)

  def update(id: ModifierId,
             idsToRemove: Seq[ByteArrayWrapper],
             toInsert: Seq[(ByteArrayWrapper, ByteArrayWrapper)]): Unit = {
    db.update(
      ByteArrayWrapper(id),
      idsToRemove,
      toInsert)
  }

  def updateWithReplacement(id: ModifierId,
                            idsToRemove: Seq[ByteArrayWrapper],
                            toInsert: Seq[(ByteArrayWrapper, ByteArrayWrapper)]): Unit = {
    db.update(ByteArrayWrapper(Algos.hash(id)), idsToRemove, Seq())
    db.update(ByteArrayWrapper(id), Seq(), toInsert)
  }

  def getAndUnpackComplexValue(key: ByteArrayWrapper, unitLen: Int): Option[Seq[Array[Byte]]] =
    db.get(key).map { v =>
      v.data.sliding(unitLen, unitLen).foldLeft(Seq[Array[Byte]]())(_ :+ _)
        .ensuring(v.data.length % unitLen == 0, "Value is inconsistent.")
    }

  def getRawValue(key: ByteArrayWrapper): Option[Array[Byte]] = db.get(key).map(_.data)

  override def close(): Unit = {
    log.info("Closing history storage...")
    db.close()
  }
}
