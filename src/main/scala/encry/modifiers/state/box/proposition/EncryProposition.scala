package encry.modifiers.state.box.proposition

import encry.modifiers.state.box.Context
import encry.modifiers.state.box.proof.Proof
import scorex.core.transaction.box.proposition.Proposition

import scala.util.Try

// TODO: JsonEncoder.
trait EncryProposition extends Proposition {

  def unlockTry(proof: Proof)(implicit ctx: Context): Try[Unit]
}
