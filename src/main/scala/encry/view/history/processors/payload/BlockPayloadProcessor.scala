package encry.view.history.processors.payload

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.ADProofs
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.settings.Algos
import encry.view.history.processors.BlockProcessor
import encry.view.history.storage.HistoryStorage
import scorex.core.consensus.History.ProgressInfo

import scala.util.{Failure, Success, Try}

trait BlockPayloadProcessor extends BaseBlockPayloadProcessor with BlockProcessor {

  protected val historyStorage: HistoryStorage

  protected val adState: Boolean

  override protected def process(payload: EncryBlockPayload): ProgressInfo[EncryPersistentModifier] = {
    historyStorage.modifierById(payload.headerId) match {
      case Some(header: EncryBlockHeader) =>
        historyStorage.modifierById(header.adProofsId) match {
          case _ if !isValidFirstBlock(header) && bestBlockIdOpt.isEmpty =>
            //TODO light mode when start from different block ?
            putToHistory(payload)
          case Some(adProof: ADProofs) =>
            processBlock(EncryBlock(header, payload, Some(adProof)), payloadIsNew = true)
          case None if !adState =>
            processBlock(EncryBlock(header, payload, None), payloadIsNew = true)
          case _ =>
            putToHistory(payload)
        }
      case _ =>
        throw new Error(s"Header for modifier $payload is undefined")
    }
  }

  override protected def validate(m: EncryBlockPayload): Try[Unit] = {
    if(historyStorage.containsObject(m.id)) {
      Failure(new Error(s"Modifier $m is already in history"))
    } else {
      historyStorage.modifierById(m.headerId) match {
        case None =>
          Failure(new Error(s"Header for modifier $m is undefined"))
        case Some(header: EncryBlockHeader) if !(header.transactionsRoot sameElements m.digest) =>
          Failure(new Error(s"Header transactions root ${Algos.encode(header.adProofsRoot)} differs from $m digest"))
        case Some(_: EncryBlockHeader) =>
          Success()
      }
    }
  }

  private def putToHistory(payload: EncryBlockPayload): ProgressInfo[EncryPersistentModifier] = {
    historyStorage.insertObjects(Seq(payload))
    ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
  }
}
