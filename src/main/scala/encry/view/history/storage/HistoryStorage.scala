package encry.view.history.storage

import com.typesafe.scalalogging.StrictLogging
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.HistoryModifierSerializer
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.storage.{EncryStorage, VersionalStorage}
import encry.utils.CoreTaggedTypes.ModifierId
import org.encryfoundation.common.serialization.Serializer
import scorex.utils.{Random => ScorexRandom}

import scala.util.{Failure, Success}

class HistoryStorage(override val store: VersionalStorage) extends EncryStorage with StrictLogging {

  def modifierById(id: ModifierId): Option[EncryPersistentModifier] =
    store.get(StorageKey @@ id.untag(ModifierId)).flatMap { res =>
      HistoryModifierSerializer.parseBytes(res) match {
        case Success(b) => Some(b)
        case Failure(e) =>
          logger.warn(s"Failed to parse block from db: $e")
          None
      }
    }

  def insertObjects(objectsToInsert: Seq[EncryPersistentModifier]): Unit =
    insert(
      StorageVersion @@ objectsToInsert.head.id.untag(ModifierId),
      objectsToInsert.map(obj =>
        StorageKey @@ obj.id.untag(ModifierId) -> StorageValue @@ HistoryModifierSerializer.toBytes(obj)
      ).toList,
    )

  def bulkInsert(version: Array[Byte],
                 indexesToInsert: Seq[(Array[Byte], Array[Byte])],
                 objectsToInsert: Seq[EncryPersistentModifier]): Unit = {
    insert(
      StorageVersion @@ version,
      (indexesToInsert.map{case (key, value) =>
        StorageKey @@ key -> StorageValue @@ value
      } ++ objectsToInsert.map(obj =>
        StorageKey @@ obj.id.untag(ModifierId) -> StorageValue @@ HistoryModifierSerializer.toBytes(obj)
      )).toList
    )
  }

  def containsObject(id: ModifierId): Boolean = store.get(StorageKey @@ id.untag(ModifierId)).isDefined

  def removeObjects(ids: Seq[ModifierId]): Unit =
    store.insert(
      StorageVersion @@ ScorexRandom.randomBytes(),
      toInsert = List.empty,
      ids.map(elem => StorageKey @@ elem.untag(ModifierId)).toList
    )

  def serializer: Serializer[EncryPersistentModifier] = HistoryModifierSerializer
}
