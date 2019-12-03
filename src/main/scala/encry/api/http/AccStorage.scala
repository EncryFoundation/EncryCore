package encry.api.http

import java.io.File
import cats.syntax.either._
import com.typesafe.scalalogging.StrictLogging
import encry.settings.EncryAppSettings
import encry.storage.levelDb.versionalLevelDB.LevelDbFactory
import org.encryfoundation.common.utils.Algos
import org.iq80.leveldb.{DB, Options}

trait AccStorage extends StrictLogging with AutoCloseable {

  val storage: DB

  def getPassword: Either[Throwable, String] =
    Either.catchNonFatal {
      new String(storage.get(AccStorage.PasswordKey)).mkString
    }

  def putPassword(pass: String): Either[Throwable, Unit] = {
    val batch = storage.createWriteBatch()
    try {
      batch.put(AccStorage.PasswordKey, pass.getBytes())
      storage.write(batch).asRight[Throwable]
    } catch {
      case err: Throwable => err.asLeft[Unit]
    }
    finally {
      batch.close()
    }
  }

  override def close(): Unit = storage.close()

}

object AccStorage extends StrictLogging {
  val PasswordKey: Array[Byte] = Algos.hash("Password_Key")

  def getDirStorage(settings: EncryAppSettings): File = new File(s"${settings.directory}/userKeys")

  def init(settings: EncryAppSettings): AccStorage = new AccStorage {
    override val storage: DB = LevelDbFactory.factory.open(getDirStorage(settings), new Options)
  }

}
