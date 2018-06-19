package encry.view.history.storage

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.HistoryModifierSerializer
import encry.storage.EncryBaseStorage
import encry.view.ObjectsStore
import io.iohk.iodb.{ByteArrayWrapper, Store}
import scorex.core.ModifierId
import scorex.core.serialization.Serializer

import scala.util.{Failure, Success}

class HistoryStorage(override val store: Store, val objectsStore: ObjectsStore) extends EncryBaseStorage {

  def modifierById(id: ModifierId): Option[EncryPersistentModifier] =
    objectsStore.get(id).flatMap { bytes =>
      HistoryModifierSerializer.parseBytes(bytes) match {
        case Success(b) => Some(b)
        case Failure(e) =>
          log.warn(s"Failed to parse block from db: ", e)
          None
      }
    }

  def insertObjects(objectsToInsert: Seq[EncryPersistentModifier]): Unit =
    objectsToInsert.foreach(o => objectsStore.put(o))

  def bulkInsert(version: ByteArrayWrapper,
                 indexesToInsert: Seq[(ByteArrayWrapper, ByteArrayWrapper)],
                 objectsToInsert: Seq[EncryPersistentModifier]): Unit = {
    insertObjects(objectsToInsert)
    insert(version, indexesToInsert)
  }

  def containsObject(id: ModifierId): Boolean = objectsStore.get(id).isDefined

  def removeObjects(ids: Seq[ModifierId]): Unit = ids.foreach(id => objectsStore.delete(id))

  def serializer: Serializer[EncryPersistentModifier] = HistoryModifierSerializer
}
