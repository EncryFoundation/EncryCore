package encry.view.history.processors.proofs

import encry.view.history.processors.{BlockProcessor, ValidationError}
import encry.consensus.History.ProgressInfo
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{ADProofs, Block, Header, Payload}

trait FullProofProcessor extends BaseADProofProcessor with BlockProcessor {

  protected val adState: Boolean

  override protected def process(m: ADProofs): ProgressInfo[PersistentModifier] =
    getBlockByProofs(m).map(block => processBlock(block, m)).getOrElse {
      historyStorage.insertObjects(Seq(m))
      ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
    }

  private def getBlockByProofs(proofs: ADProofs): Option[Block] =
    typedModifierById[Header](proofs.headerId).flatMap { h =>
      typedModifierById[Payload](h.payloadId).map(p => Block(h, p, if (adState) Some(proofs) else None))
    }

  override protected def validate(m: ADProofs): Either[ValidationError, PersistentModifier] =
    modifierValidation(m, typedModifierById[Header](m.headerId))
}
