package encry.view.fast.sync

import com.typesafe.scalalogging.StrictLogging
import org.iq80.leveldb.{ DB, DBIterator, ReadOptions, WriteBatch }

trait DBTryCatchFinallyProvider extends StrictLogging {

  val storage: DB

  def readWrite[Output](f: (WriteBatch, ReadOptions, DBIterator) => Output, onFailure: Throwable => Output): Output = {
    val snapshot             = storage.getSnapshot
    val readOptions          = new ReadOptions().snapshot(snapshot)
    val batch: WriteBatch    = storage.createWriteBatch()
    val iterator: DBIterator = storage.iterator(readOptions)
    try {
      val output = f(batch, readOptions, iterator)
      storage.write(batch)
      output
    } catch {
      case error: Throwable =>
        logger.info(s"Error has occurred $error")
        onFailure(error)
    } finally {
      iterator.close()
      readOptions.snapshot().close()
      batch.close()
    }
  }
}
