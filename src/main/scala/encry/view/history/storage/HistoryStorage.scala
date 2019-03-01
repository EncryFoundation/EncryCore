package encry.view.history.storage

import com.typesafe.scalalogging.StrictLogging
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.HistoryModifierSerializer
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.storage.iodb.versionalIODB.IODBHistoryWrapper
import encry.storage.levelDb.versionalLevelDB.VLDBWrapper
import encry.storage.{EncryStorage, VersionalStorage}
import encry.utils.CoreTaggedTypes.ModifierId
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.serialization.Serializer
import scorex.utils.{Random => ScorexRandom}

import scala.util.{Failure, Random, Success}

case class HistoryStorage(override val store: VersionalStorage) extends EncryStorage with StrictLogging {

  def modifierById(id: ModifierId): Option[EncryPersistentModifier] = {
    val possibleMod = store match {
      case iodb: IODBHistoryWrapper =>
        iodb.objectStore.get(ByteArrayWrapper(id)).map(_.data)
      case _: VLDBWrapper =>
        store.get(StorageKey @@ id.untag(ModifierId))
    }
    possibleMod.flatMap { res =>
      HistoryModifierSerializer.parseBytes(res) match {
        case Success(b) => Some(b)
        case Failure(e) =>
          logger.warn(s"Failed to parse block from db: $e")
          None
      }
    }
  }

  def insertObjects(objectsToInsert: Seq[EncryPersistentModifier]): Unit =
    store match {
      case iodb: IODBHistoryWrapper =>
        iodb.objectStore.update(Random.nextLong(), Seq.empty,
          objectsToInsert.map(obj => ByteArrayWrapper(obj.id) -> ByteArrayWrapper(HistoryModifierSerializer.toBytes(obj))))
      case _: VLDBWrapper =>
        insert(
          StorageVersion @@ objectsToInsert.head.id.untag(ModifierId),
          objectsToInsert.map(obj =>
            StorageKey @@ obj.id.untag(ModifierId) -> StorageValue @@ HistoryModifierSerializer.toBytes(obj)
          ).toList,
        )
    }


  def bulkInsert(version: Array[Byte],
                 indexesToInsert: Seq[(Array[Byte], Array[Byte])],
                 objectsToInsert: Seq[EncryPersistentModifier]): Unit = {
    store match {
      case _: IODBHistoryWrapper =>
        insertObjects(objectsToInsert)
        insert(
          StorageVersion @@ version,
          indexesToInsert.map{ case (key, value) =>
            StorageKey @@ key -> StorageValue @@ value
          }.toList
        )
      case _: VLDBWrapper =>
        insert(
          StorageVersion @@ version,
          (indexesToInsert.map{case (key, value) =>
            StorageKey @@ key -> StorageValue @@ value
          } ++ objectsToInsert.map(obj =>
            StorageKey @@ obj.id.untag(ModifierId) -> StorageValue @@ HistoryModifierSerializer.toBytes(obj)
          )).toList
        )
    }
  }

  def containsObject(id: ModifierId): Boolean =
    store match {
      case iodb: IODBHistoryWrapper =>
        iodb.objectStore.get(ByteArrayWrapper(id)).isDefined
      case _: VLDBWrapper =>
        store.get(StorageKey @@ id.untag(ModifierId)).isDefined
    }

  def removeObjects(ids: Seq[ModifierId]): Unit =
    store match {
      case iodb: IODBHistoryWrapper =>
        iodb.objectStore.update(Random.nextLong(), ids.map(ByteArrayWrapper.apply), Seq.empty)
      case _: VLDBWrapper =>
        store.insert(
          StorageVersion @@ ScorexRandom.randomBytes(),
          toInsert = List.empty,
          ids.map(elem => StorageKey @@ elem.untag(ModifierId)).toList
        )
    }

  def serializer: Serializer[EncryPersistentModifier] = HistoryModifierSerializer
}
