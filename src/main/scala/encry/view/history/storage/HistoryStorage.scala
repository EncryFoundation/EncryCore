package encry.view.history.storage

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.HistoryModifierSerializer
import io.iohk.iodb.{ByteArrayWrapper, Store}
import scorex.core.ModifierId
import scorex.core.serialization.Serializer
import scorex.core.utils.ScorexLogging

import scala.util.{Failure, Success}

class HistoryStorage(val db: Store) extends ScorexLogging with AutoCloseable {

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

  def contains(id: ModifierId): Boolean = db.get(ByteArrayWrapper(id)).isDefined

  def insert(id: ModifierId, toInsert: Seq[(ByteArrayWrapper, ByteArrayWrapper)]): Unit = update(id, Seq(), toInsert)

  def update(id: ModifierId,
             idsToRemove: Seq[ByteArrayWrapper],
             toInsert: Seq[(ByteArrayWrapper, ByteArrayWrapper)]): Unit = {
    db.update(
      ByteArrayWrapper(id),
      idsToRemove,
      toInsert)
  }

  override def close(): Unit = {
    log.info("Closing history storage...")
    db.close()
  }

  def serializer: Serializer[EncryPersistentModifier] = HistoryModifierSerializer
}
