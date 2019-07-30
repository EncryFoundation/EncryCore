package encry.view.history.storage

import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.storage.iodb.versionalIODB.IODBHistoryWrapper
import encry.storage.levelDb.versionalLevelDB.VLDBWrapper
import encry.storage.VersionalStorage
import scorex.utils.{Random => ScorexRandom}
import encry.storage.EncryStorage
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.HistoryModifiersProtoSerializer
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
import scala.util.{Failure, Random, Success}

final case class HistoryStorage(override val store: VersionalStorage) extends EncryStorage {

  def modifierById(id: ModifierId): Option[PersistentModifier] = {
    val modBytesWithTypeId = store match {
      case iodb: IODBHistoryWrapper => iodb.objectStore.get(ByteArrayWrapper(id)).map(_.data)
      case _: VLDBWrapper           => store.get(StorageKey @@ id.untag(ModifierId))
    }
    modBytesWithTypeId.flatMap(res => HistoryModifiersProtoSerializer.fromProto(res) match {
      case Success(b) => Some(b)
      case Failure(e) => logger.warn(s"Failed to parse block from db: $e"); None
    })
  }

  def containsMod(id: ModifierId): Boolean = store match {
    case iodb: IODBHistoryWrapper => iodb.objectStore.get(ByteArrayWrapper(id)).isDefined
    case _: VLDBWrapper           => store.contains(StorageKey @@ id.untag(ModifierId))
  }

  def modifiersBytesById(id: ModifierId): Option[Array[Byte]] = store match {
    case iodb: IODBHistoryWrapper => iodb.objectStore.get(ByteArrayWrapper(id)).map(_.data.tail)
    case _: VLDBWrapper           => store.get(StorageKey @@ id.untag(ModifierId)).map(_.tail)
  }

  def insertObjects(objectsToInsert: Seq[PersistentModifier]): Unit = store match {
    case iodb: IODBHistoryWrapper => iodb.objectStore
      .update(
        Random.nextLong(),
        Seq.empty,
        objectsToInsert
          .map(obj => ByteArrayWrapper(obj.id) -> ByteArrayWrapper(HistoryModifiersProtoSerializer.toProto(obj)))
      )
    case _: VLDBWrapper =>
      insert(
        StorageVersion @@ objectsToInsert.head.id.untag(ModifierId),
        objectsToInsert
          .map(obj => StorageKey @@ obj.id.untag(ModifierId) -> StorageValue @@ HistoryModifiersProtoSerializer.toProto(obj))
          .toList
      )
  }


  def bulkInsert(version: Array[Byte],
                 indexesToInsert: Seq[(Array[Byte], Array[Byte])],
                 objectsToInsert: Seq[PersistentModifier]): Unit = store match {
    case _: IODBHistoryWrapper =>
      insertObjects(objectsToInsert)
      insert(
        StorageVersion @@ version,
        indexesToInsert.map { case (key, value) => StorageKey @@ key -> StorageValue @@ value }.toList
      )
    case _: VLDBWrapper =>
      insert(
        StorageVersion @@ version,
        (indexesToInsert.map { case (key, value) => StorageKey @@ key -> StorageValue @@ value } ++
          objectsToInsert.map(obj =>
            StorageKey @@ obj.id.untag(ModifierId) -> StorageValue @@ HistoryModifiersProtoSerializer.toProto(obj)
          )).toList
      )
  }

  def removeObjects(ids: Seq[ModifierId]): Unit = store match {
    case iodb: IODBHistoryWrapper =>
      iodb.objectStore.update(Random.nextLong(), ids.map(ByteArrayWrapper.apply), Seq.empty)
    case _: VLDBWrapper =>
      store.insert(
        StorageVersion @@ ScorexRandom.randomBytes(),
        toInsert = List.empty,
        ids.map(elem => StorageKey @@ elem.untag(ModifierId)).toList
      )
  }
}