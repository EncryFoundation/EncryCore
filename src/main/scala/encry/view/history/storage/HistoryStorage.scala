package encry.view.history.storage

import com.typesafe.scalalogging.StrictLogging
import encry.utils.CoreTaggedTypes.ModifierId
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.HistoryModifierSerializer
import encry.storage.EncryStorage
import encry.storage.levelDb.versionalLevelDB.{LevelDbElem, VersionalLevelDB}
import encry.storage.levelDb.versionalLevelDB.VersionalLevelDBCompanion.{LevelDBVersion, VersionalLevelDbKey}
import io.iohk.iodb.{ByteArrayWrapper, Store}
import scorex.utils.{Random => ScorexRandom}
import org.encryfoundation.common.serialization.Serializer

import scala.util.{Failure, Random, Success}

class HistoryStorage(override val store: VersionalLevelDB) extends EncryStorage with StrictLogging {

  def modifierById(id: ModifierId): Option[EncryPersistentModifier] =
    store.get(VersionalLevelDbKey @@ id.untag(ModifierId)).flatMap { res =>
      HistoryModifierSerializer.parseBytes(res) match {
        case Success(b) => Some(b)
        case Failure(e) =>
          logger.warn(s"Failed to parse block from db: $e")
          None
      }
    }

  def insertObjects(objectsToInsert: Seq[EncryPersistentModifier]): Unit =
    insert(
      ByteArrayWrapper(objectsToInsert.head.id),
      objectsToInsert.map(obj => ByteArrayWrapper(obj.id) -> ByteArrayWrapper(HistoryModifierSerializer.toBytes(obj))),
    )

  def bulkInsert(version: ByteArrayWrapper,
                 indexesToInsert: Seq[(ByteArrayWrapper, ByteArrayWrapper)],
                 objectsToInsert: Seq[EncryPersistentModifier]): Unit = {
    insert(version, indexesToInsert ++
      objectsToInsert.map(obj => ByteArrayWrapper(obj.id) -> ByteArrayWrapper(HistoryModifierSerializer.toBytes(obj))))
  }

  def containsObject(id: ModifierId): Boolean = store.get(VersionalLevelDbKey @@ id.untag(ModifierId)).isDefined

  def removeObjects(ids: Seq[ModifierId]): Unit =
    store.insert(
      LevelDbElem(
        LevelDBVersion @@ ScorexRandom.randomBytes(),
        List.empty,
        ids.map(elem => VersionalLevelDbKey @@ elem.untag(ModifierId))
      )
    )

  def serializer: Serializer[EncryPersistentModifier] = HistoryModifierSerializer
}
