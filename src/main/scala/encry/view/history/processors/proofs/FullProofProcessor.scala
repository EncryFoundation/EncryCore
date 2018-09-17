package encry.view.history.processors.proofs

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.{ADProofs, Block, Header, Payload}
import encry.view.history.processors.BlockProcessor
import encry.consensus.History.ProgressInfo

import scala.util.Try

trait FullProofProcessor extends BaseADProofProcessor with BlockProcessor {

  protected val adState: Boolean

  override protected def process(m: ADProofs): ProgressInfo[EncryPersistentModifier] =
    getBlockByProofs(m).map(block => processBlock(block, m)).getOrElse {
      historyStorage.insertObjects(Seq(m))
      ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
    }

  private def getBlockByProofs(proofs: ADProofs): Option[Block] =
    typedModifierById[Header](proofs.headerId).flatMap { h =>
      typedModifierById[Payload](h.payloadId).map(p => Block(h, p, if (adState) Some(proofs) else None))
    }

  override protected def validate(m: ADProofs): Try[Unit] =
    modifierValidation(m, typedModifierById[Header](m.headerId))
}
