package encry.api.http

import java.io.File
import com.typesafe.scalalogging.StrictLogging
import encry.settings.EncryAppSettings
import encry.storage.levelDb.versionalLevelDB.LevelDbFactory
import cats.syntax.either._
import org.encryfoundation.common.utils.Algos
import org.iq80.leveldb.{ DB, Options }

trait AccStorage extends StrictLogging with AutoCloseable {

  val storage: DB

  def getPassword: Either[Throwable, String] =
    Either.catchNonFatal {
      storage.get(AccStorage.PasswordKey).map(_.toChar).mkString
    }

  def putPassword(pass: String): Either[Throwable, Unit] =
    Either.catchNonFatal {
      storage.put(AccStorage.PasswordKey, pass.getBytes())
    }

  override def close(): Unit = storage.close()

}

object AccStorage extends StrictLogging {
  val PasswordKey: Array[Byte] = Algos.hash("Password_Key")

  def getDirStorage(settings: EncryAppSettings): File = new File(s"${settings.directory}/ackstorage")

  def init(settings: EncryAppSettings): AccStorage = new AccStorage {
    override val storage: DB = {
      LevelDbFactory.factory.open(getDirStorage(settings), new Options)
    }
  }

}
