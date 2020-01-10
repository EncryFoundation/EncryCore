package encry.api.http

import java.io.File
import cats.syntax.either._
import com.typesafe.scalalogging.StrictLogging
import encry.settings.EncryAppSettings
import encry.storage.VersionalStorage.StorageKey
import encry.storage.levelDb.versionalLevelDB.LevelDbFactory
import org.encryfoundation.common.utils.Algos
import org.iq80.leveldb.{DB, Options}
import scorex.utils.Random
import supertagged.TaggedType

trait AccStorage extends StrictLogging with AutoCloseable {

  val storage: DB

  val verifyPassword: String => Boolean = pass => {
    val salt = storage.get(AccStorage.SaltKey)
    val passHash = storage.get(AccStorage.PasswordHashKey)
    Algos.hash(pass.getBytes() ++ salt) sameElements passHash
  }

  def setPassword(pass: String): Either[Throwable, Unit] = {
    val batch = storage.createWriteBatch()
    val salt = Random.randomBytes()
    try {
      batch.put(AccStorage.PasswordHashKey, Algos.hash(pass.getBytes() ++ salt))
      batch.put(AccStorage.SaltKey, salt)
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

  object PasswordHash extends TaggedType[Array[Byte]]
  object PasswordSalt extends TaggedType[Array[Byte]]

  type PasswordHash = PasswordHash.Type
  type PasswordSalt = PasswordSalt.Type

  val PasswordHashKey: StorageKey = StorageKey @@ Algos.hash("Password_Key")
  val SaltKey: StorageKey = StorageKey @@ Algos.hash("Salt_Key")

  def getDirStorage(settings: EncryAppSettings): File = new File(s"${settings.directory}/userKeys")

  def init(settings: EncryAppSettings): AccStorage = new AccStorage {
    override val storage: DB = LevelDbFactory.factory.open(getDirStorage(settings), new Options)
  }

}
