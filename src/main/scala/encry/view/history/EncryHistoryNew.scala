package encry.view.history

import encry.view.history.storage.HistoryStorage
import org.encryfoundation.common.modifiers.history.{Header, Payload}
import org.encryfoundation.common.utils.TaggedTypes.{Difficulty, ModifierId}

final case class EncryHistoryNew(storage: HistoryStorage) extends HistoryExtension {
  override def realDifficulty(h: Header): Difficulty = ???

  override def isNewHeader(header: Header): Boolean = ???

  override val history: HistoryStorage = _
}

object EncryHistoryNew {

}