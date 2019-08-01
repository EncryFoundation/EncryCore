package encry.view.history.processors.payload

import encry.consensus.History.ProgressInfo
import encry.view.history.processors.{BlockProcessor, ValidationError}
import encry.view.history.storage.HistoryStorage
import encry.settings.EncryAppSettings
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{Block, Header, Payload}

trait BlockPayloadProcessor extends BlockProcessor {

   val settings: EncryAppSettings

   val historyStorage: HistoryStorage


}