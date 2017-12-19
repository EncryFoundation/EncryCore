package encry.view.history.storage.processors

import encry.modifiers.EncryPersistentModifier
import encry.view.history.storage.HistoryStorage
import scorex.core.utils.ScorexLogging

trait EncryBaseProcessor[PMOD <: EncryPersistentModifier] extends ScorexLogging {

  protected val historyStorage: HistoryStorage

}
