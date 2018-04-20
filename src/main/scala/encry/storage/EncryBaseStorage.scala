package encry.storage

import encry.settings.Algos
import encry.storage.codec.FixLenComplexValueCodec
import io.iohk.iodb.{ByteArrayWrapper, Store}
import scorex.core.utils.ScorexLogging

trait EncryBaseStorage extends AutoCloseable with ScorexLogging {

  val store: Store

  def insert(version: ByteArrayWrapper,
             toInsert: Seq[(ByteArrayWrapper, ByteArrayWrapper)]): Unit =
      store.update(version, Seq.empty, toInsert)

  def remove(version: ByteArrayWrapper,
             toRemove: Seq[ByteArrayWrapper]): Unit =
    store.update(version, toRemove, Seq.empty)

  def update(version: ByteArrayWrapper,
             toRemove: Seq[ByteArrayWrapper],
             toUpdate: Seq[(ByteArrayWrapper, ByteArrayWrapper)]): Unit = {
    remove(version, toRemove)
    insert(ByteArrayWrapper(Algos.hash(version.data)), toUpdate)
  }

  def get(key: ByteArrayWrapper): Option[Array[Byte]] = store.get(key).map(_.data)

  def readComplexValue(key: ByteArrayWrapper, unitLen: Int): Option[Seq[Array[Byte]]] =
    store.get(key).flatMap { v =>
      FixLenComplexValueCodec.parseComplexValue(v.data, unitLen).toOption
    }

  override def close(): Unit = {
    log.info("Closing storage")
    store.close()
  }
}
