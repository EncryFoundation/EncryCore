package encry.view.history.processors.payload

import encry.consensus.History.ProgressInfo
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.{ADProofs, Block, Header, Payload}
import encry.view.history.processors.BlockProcessor
import encry.view.history.storage.HistoryStorage
import encry.settings.EncryAppSettings
import org.encryfoundation.common.Algos

import scala.util.Try

trait BlockPayloadProcessor extends BaseBlockPayloadProcessor with BlockProcessor {

  protected val settings: EncryAppSettings

  protected val historyStorage: HistoryStorage

  protected val adState: Boolean

  override protected def process(payload: Payload): ProgressInfo[EncryPersistentModifier] =
    getBlockByPayload(payload)
      .flatMap { block =>
        logger.info(s"Block: ${Algos.encode(block.id)}")
        if (block.header.height - bestBlockHeight >= 2 + settings.network.maxInvObjects) None
        else Some(processBlock(block, payload))
      }.getOrElse(putToHistory(payload))

  private def getBlockByPayload(payload: Payload): Option[Block] = {
    typedModifierById[Header](payload.headerId).flatMap { h =>
      logger.info(s"Get header in payload: ${Algos.encode(h.id)}")
      if (!adState) Some(Block(h, payload, None))
      else typedModifierById[ADProofs](h.adProofsId).map(ps => Block(h, payload, Some(ps)))
    }
  }

  override protected def validate(m: Payload): Try[Unit] = {
    logger.info(s"valid payload: ${Algos.encode(m.id)}")
    logger.info(s"header: ${typedModifierById[Header](m.headerId).map(h => Algos.encode(m.id))}")
    modifierValidation(m, typedModifierById[Header](m.headerId))
  }

  private def putToHistory(payload: Payload): ProgressInfo[EncryPersistentModifier] = {
    historyStorage.insertObjects(Seq(payload))
    ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
  }
}
