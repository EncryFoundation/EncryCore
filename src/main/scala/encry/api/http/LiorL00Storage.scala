package encry.api.http

import java.io.File
import com.typesafe.scalalogging.StrictLogging
import encry.settings.EncryAppSettings
import encry.storage.levelDb.versionalLevelDB.LevelDbFactory
import cats.syntax.either._
import org.encryfoundation.common.utils.Algos
import org.iq80.leveldb.{ DB, Options }

trait LiorL00Storage extends StrictLogging with AutoCloseable {

  val storage: DB

  def getPassword: Either[Throwable, String] =
    Either.catchNonFatal {
      logger.info(s"User liorL00 get password from db.")
      val a = storage.get(LiorL00Storage.PasswordKey).map(_.toChar).mkString
      logger.info(s"Password of lior l 00 is $a")
      a
    }

  def putPassword(pass: String): Either[Throwable, Unit] =
    Either.catchNonFatal {
      logger.info(s"Lior l00 put password to db")
      storage.put(LiorL00Storage.PasswordKey, pass.getBytes())
      logger.info(s"Put is good")
    }

  override def close(): Unit = storage.close()

}

object LiorL00Storage extends StrictLogging {
  val PasswordKey: Array[Byte] = Algos.hash("Password_Key")

  def getDirLiorLOO(settings: EncryAppSettings): File = new File(s"${settings.directory}/liorL00Dir")

  def init(settings: EncryAppSettings): LiorL00Storage = new LiorL00Storage {
    override val storage: DB = {
      logger.info("Init liorl000 dir with levelDB storage")
      LevelDbFactory.factory.open(getDirLiorLOO(settings), new Options)
    }
  }

}
