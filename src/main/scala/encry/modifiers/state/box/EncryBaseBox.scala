package encry.modifiers.state.box

import encry.modifiers.state.box.body.BaseBoxBody
import scorex.core.serialization.JsonSerializable
import scorex.core.transaction.box.proposition.Proposition
import scorex.crypto.authds.ADKey
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.Box.Amount

trait EncryBaseBox[P <: Proposition, BB <: BaseBoxBody] extends Box[P] with JsonSerializable {
  override val proposition: P
  override val id: ADKey
  val body: BB

  // Remove redundant field from base class.
  override val value: Amount = 0L
}

object EncryBaseBox extends Serializable