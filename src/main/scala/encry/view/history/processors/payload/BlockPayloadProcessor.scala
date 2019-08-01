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

  protected def process(payload: Payload): ProgressInfo[PersistentModifier] = getBlockByPayload(payload)
    .flatMap(block =>
      if (block.header.height - getBestBlockHeight >= 2 + settings.network.maxInvObjects) None
      else Some(processBlock(block, payload))
    )
    .getOrElse(putToHistory(payload))

  private def getBlockByPayload(payload: Payload): Option[Block] = getHeaderById(payload.headerId)
    .flatMap(h => Some(Block(h, payload)))



  private def putToHistory(payload: Payload): ProgressInfo[PersistentModifier] = {
    historyStorage.insertObjects(Seq(payload))
    ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
  }
}