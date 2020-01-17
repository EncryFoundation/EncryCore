package encry.api.http

import java.io.{ ByteArrayInputStream, ByteArrayOutputStream, File, ObjectInputStream, ObjectOutputStream }
import cats.syntax.either._
import com.typesafe.scalalogging.StrictLogging
import encry.settings.EncryAppSettings
import org.encryfoundation.common.utils.Algos
import org.iq80.leveldb.{ DB, Options }
import scorex.crypto.hash.Digest32
import encry.storage.levelDb.versionalLevelDB.LevelDbFactory

trait SettingsStorage extends StrictLogging with AutoCloseable {

  val storage: DB

  def getSettings: Option[EncryAppSettings] = SettingsStorage.deserialise(storage.get(SettingsStorage.SettingsKey))

  def putSettings(settings: EncryAppSettings): Either[Throwable, Unit] = {
    println(s"settings = ${settings.network.nodeName}")
    val batch                           = storage.createWriteBatch()
    val serialisedSettings: Array[Byte] = SettingsStorage.serialise(settings)
    try {
      batch.put(SettingsStorage.SettingsKey, serialisedSettings)
      storage.write(batch).asRight[Throwable]
    } catch {
      case err: Throwable => err.asLeft[Unit]
    } finally {
      batch.close()
    }
  }

  override def close(): Unit = storage.close()

}

object SettingsStorage extends StrictLogging {

  val SettingsKey: Digest32 = Algos.hash(s"Settings_Key")

  def serialise(value: EncryAppSettings): Array[Byte] = {
    val stream: ByteArrayOutputStream = new ByteArrayOutputStream()
    val oos                           = new ObjectOutputStream(stream)
    try {
      oos.writeObject(value)
      stream.toByteArray
    } catch {
      case err: Throwable => throw new Exception(s"Error during serialisation cause of $err")
    } finally {
      oos.close()
    }
  }

  def deserialise(bytes: Array[Byte]): Option[EncryAppSettings] = {
    val ois = new ObjectInputStream(new ByteArrayInputStream(bytes))
    try {
      val value = ois.readObject
      val deserialised: Option[EncryAppSettings] = value match {
        case set: EncryAppSettings =>
          logger.info(s"Deserialisation ended successfully")
          Some(set)
      }
      deserialised
    } catch {
      case err: Throwable => throw new Exception(s"Error during serialisation cause of $err")
    } finally {
      ois.close()
    }
  }

  def getDirStorage(settings: EncryAppSettings): File = new File(s"${settings.directory}/userSettings")

  def init(settings: EncryAppSettings): SettingsStorage = new SettingsStorage {
    override val storage: DB = LevelDbFactory.factory.open(getDirStorage(settings), new Options)
  }

}
