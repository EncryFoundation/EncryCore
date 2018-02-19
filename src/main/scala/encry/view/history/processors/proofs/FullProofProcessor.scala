package encry.view.history.processors.proofs

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.ADProofs
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.view.history.processors.BlockProcessor
import scorex.core.consensus.History.ProgressInfo
import scorex.crypto.encode.Base58

import scala.util.{Failure, Success, Try}

trait FullProofProcessor extends BaseADProofProcessor with BlockProcessor {

  protected val adState: Boolean

  override protected def process(m: ADProofs): ProgressInfo[EncryPersistentModifier] = {
    historyStorage.modifierById(m.headerId) match {
      case Some(header: EncryBlockHeader) =>
        historyStorage.modifierById(header.payloadId) match {
          case Some(payload: EncryBlockPayload) if adState =>
            processBlock(EncryBlock(header, payload, Some(m)), isNewerPayload = false)
          case _ =>
            historyStorage.insertObjects(Seq(m))
            ProgressInfo(None, Seq.empty, None, Seq.empty)
        }
      case _ =>
        throw new Error(s"Header for modifier $m is no defined")
    }
  }

  override protected def validate(m: ADProofs): Try[Unit] = {
    if (historyStorage.containsObject(m.id)) {
      Failure(new Error(s"Modifier $m is already in history"))
    } else {
      historyStorage.modifierById(m.headerId) match {
        case None =>
          Failure(new Error(s"Header for modifier $m is no defined"))
        case Some(header: EncryBlockHeader) if !(header.adProofsRoot sameElements m.digest) =>
          Failure(new Error(s"Header ADProofs root ${Base58.encode(header.adProofsRoot)} differs from $m digest"))
        case Some(_: EncryBlockHeader) =>
          Success()
      }
    }
  }
}
