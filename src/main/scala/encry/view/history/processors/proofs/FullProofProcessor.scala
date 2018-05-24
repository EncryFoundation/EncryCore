package encry.view.history.processors.proofs

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.ADProofs
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.view.history.processors.BlockProcessor
import scorex.core.consensus.History.ProgressInfo

import scala.util.Try

trait FullProofProcessor extends BaseADProofProcessor with BlockProcessor {

  protected val adState: Boolean

  override protected def process(m: ADProofs): ProgressInfo[EncryPersistentModifier] = {
    historyStorage.modifierById(m.headerId) match {
      case Some(header: EncryBlockHeader) =>
        historyStorage.modifierById(header.payloadId) match {
          case Some(payload: EncryBlockPayload) if adState =>
            processBlock(EncryBlock(header, payload, Some(m)), payloadIsNew = false)
          case _ =>
            historyStorage.insertObjects(Seq(m))
            ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
        }
      case _ =>
        throw new Exception(s"Header for modifier $m is no defined")
    }
  }

  override protected def validate(m: ADProofs): Try[Unit] =
    modifierValidation(m, typedModifierById[EncryBlockHeader](m.headerId))
}
