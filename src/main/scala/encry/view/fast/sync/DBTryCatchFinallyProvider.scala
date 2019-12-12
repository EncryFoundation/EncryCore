package encry.view.fast.sync

import com.typesafe.scalalogging.StrictLogging
import org.iq80.leveldb.{ DB, DBIterator, ReadOptions, WriteBatch }
import cats.syntax.either._

trait DBTryCatchFinallyProvider extends StrictLogging {

  val storage: DB

  def readWrite[Output](
    f: (WriteBatch, ReadOptions, DBIterator) => Output
  ): Either[Throwable, Output] = {
    val snapshot             = storage.getSnapshot
    val readOptions          = new ReadOptions().snapshot(snapshot)
    val batch: WriteBatch    = storage.createWriteBatch()
    val iterator: DBIterator = storage.iterator(readOptions)
    try {
      val output = f(batch, readOptions, iterator)
      storage.write(batch)
      output.asRight[Throwable]
    } catch {
      case error: Throwable =>
        logger.info(s"Error has occurred $error")
        error.asLeft[Output]
    } finally {
      iterator.close()
      readOptions.snapshot().close()
      batch.close()
    }
  }
}
