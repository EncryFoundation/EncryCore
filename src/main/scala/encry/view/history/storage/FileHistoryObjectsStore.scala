package encry.view.history.storage

import java.nio.file.{Files, Paths, StandardOpenOption}

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.HistoryModifierSerializer
import encry.settings.Algos
import encry.view.ObjectsStore
import scorex.core.ModifierId

import scala.util.Try

class FileHistoryObjectsStore(dir: String) extends ObjectsStore {

  override def get(id: ModifierId): Option[Array[Byte]] = Try {
    Files.readAllBytes(getPath(id))
  }.toOption

  override def put(m: EncryPersistentModifier): Try[Unit] = Try {
    val p = getPath(m.id)
    p.toFile.createNewFile()
    Files.write(p, HistoryModifierSerializer.toBytes(m), StandardOpenOption.WRITE)
  }

  override def delete(id: ModifierId): Try[Unit] = Try {
    Files.delete(getPath(id))
  }

  private def getPath(id: ModifierId) = Paths.get(dir + "/" + Algos.encode(id))
}
