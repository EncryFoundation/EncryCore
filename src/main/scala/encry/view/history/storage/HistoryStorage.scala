package encry.view.history.storage

import encry.ModifierId
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.HistoryModifierSerializer
import encry.stats.LoggingActor.LogMessage
import encry.storage.EncryStorage
import encry.EncryApp.{settings, system}
import io.iohk.iodb.{ByteArrayWrapper, Store}
import org.encryfoundation.common.serialization.Serializer
import scala.util.{Failure, Random, Success}

class HistoryStorage(override val store: Store, val objectsStore: Store) extends EncryStorage {

  def modifierById(id: ModifierId): Option[EncryPersistentModifier] =
    objectsStore.get(ByteArrayWrapper(id)).flatMap { res =>
      HistoryModifierSerializer.parseBytes(res.data) match {
        case Success(b) => Some(b)
        case Failure(e) =>
          if (settings.logging.enableLogging) system.actorSelection("user/loggingActor") !
            LogMessage("Warn", s"Failed to parse block from db: $e", System.currentTimeMillis())
          None
      }
    }

  def insertObjects(objectsToInsert: Seq[EncryPersistentModifier]): Unit =
    objectsStore.update(Random.nextLong(), Seq.empty,
      objectsToInsert.map(obj => ByteArrayWrapper(obj.id) -> ByteArrayWrapper(HistoryModifierSerializer.toBytes(obj))))

  def bulkInsert(version: ByteArrayWrapper,
                 indexesToInsert: Seq[(ByteArrayWrapper, ByteArrayWrapper)],
                 objectsToInsert: Seq[EncryPersistentModifier]): Unit = {
    insertObjects(objectsToInsert)
    insert(version, indexesToInsert)
  }

  def containsObject(id: ModifierId): Boolean = objectsStore.get(ByteArrayWrapper(id)).isDefined

  def removeObjects(ids: Seq[ModifierId]): Unit = objectsStore.update(Random.nextLong(), ids.map(ByteArrayWrapper.apply), Seq.empty)

  def serializer: Serializer[EncryPersistentModifier] = HistoryModifierSerializer
}
