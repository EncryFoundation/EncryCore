package encry.view

import io.iohk.iodb.{ByteArrayWrapper, Store}
import scorex.core.utils.ScorexLogging

import scala.util.Random

trait EncryBaseStorage extends AutoCloseable with ScorexLogging {

  val db: Store

  def insert(version: ByteArrayWrapper,
             toInsert: Seq[(ByteArrayWrapper, ByteArrayWrapper)]): Unit =
    db.update(version, Seq.empty, toInsert)

  def updateWithReplacement(version: ByteArrayWrapper,
                            idsToReplace: Seq[ByteArrayWrapper],
                            toInsert: Seq[(ByteArrayWrapper, ByteArrayWrapper)]): Unit = {
    db.update(Random.nextLong(), idsToReplace, Seq())
    db.update(version, Seq(), toInsert)
  }

  def get(key: ByteArrayWrapper): Option[Array[Byte]] = db.get(key).map(_.data)

  def parseComplexValue(key: ByteArrayWrapper, unitLen: Int): Option[Seq[Array[Byte]]] =
    db.get(key).map { v =>
      v.data.sliding(unitLen, unitLen).foldLeft(Seq[Array[Byte]]())(_ :+ _)
        .ensuring(v.data.length % unitLen == 0, "Value is inconsistent.")
    }

  override def close(): Unit = {
    log.info("Closing history storage...")
    db.close()
  }
}
