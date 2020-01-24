package encry.api.http

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, File, ObjectInputStream, ObjectOutputStream}
import com.typesafe.scalalogging.StrictLogging
import encry.settings.EncryAppSettings
import org.encryfoundation.common.utils.Algos
import org.iq80.leveldb.{DB, Options}
import scorex.crypto.hash.Digest32
import encry.storage.levelDb.versionalLevelDB.LevelDbFactory

trait SettingsStorage extends StrictLogging with AutoCloseable {

  val storage: DB

  val SettingsKey: Digest32 = Algos.hash(s"Settings_Key")

  def getSettings: Option[EncryAppSettings] = deserialise(storage.get(SettingsKey))

  def putSettings(settings: EncryAppSettings): Unit = {
    val batch                           = storage.createWriteBatch()
    try {
      val serialisedSettings: Array[Byte] = serialise(settings)
      batch.put(SettingsKey, serialisedSettings)
      storage.write(batch)
    } catch {
      case err: Throwable => throw new Exception(s"Error during saving settings cause of $err")
    } finally {
      batch.close()
    }
  }

  private final def serialise(value: EncryAppSettings): Array[Byte] = {
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

  private final def deserialise(bytes: Array[Byte]): Option[EncryAppSettings] = {
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

  override def close(): Unit = storage.close()

}

object SettingsStorage extends StrictLogging {

  def getDirStorage(settings: EncryAppSettings): File = {
    val dir = new File(s"${settings.directory}/userSettings")
    dir.mkdirs()
    dir
  }


  def init(settings: EncryAppSettings): SettingsStorage = new SettingsStorage {
    override val storage: DB = LevelDbFactory.factory.open(getDirStorage(settings), new Options)
  }

}
