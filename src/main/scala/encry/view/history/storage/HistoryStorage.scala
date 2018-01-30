package encry.view.history.storage

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.HistoryModifierSerializer
import encry.view.EncryBaseStorage
import io.iohk.iodb.{ByteArrayWrapper, Store}
import scorex.core.ModifierId
import scorex.core.serialization.Serializer

import scala.util.{Failure, Success}

class HistoryStorage(val db: Store) extends EncryBaseStorage {

  def modifierById(id: ModifierId): Option[EncryPersistentModifier] =
    db.get(ByteArrayWrapper(id)).flatMap { bBytes =>
      HistoryModifierSerializer.parseBytes(bBytes.data) match {
        case Success(b) =>
          Some(b)
        case Failure(e) =>
          log.warn(s"Failed to parse block from db (bytes: ${bBytes.data.mkString("-")}): ", e)
          None
      }
    }

  def serializer: Serializer[EncryPersistentModifier] = HistoryModifierSerializer
}
