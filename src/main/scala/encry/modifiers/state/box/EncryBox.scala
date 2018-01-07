package encry.modifiers.state.box

import scorex.core.serialization.JsonSerializable
import scorex.core.transaction.box.Box.Amount
import scorex.core.transaction.box.proposition.Proposition
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Digest32

trait EncryBox[P <: Proposition] extends EncryBaseBox with JsonSerializable {

  import EncryBox._

  override val proposition: P

  val bxTypeId: BxTypeId

  val bxHash: Digest32

  override val id: ADKey = ADKey @@ (Array[Byte](bxTypeId) ++ bxHash)

  // Remove redundant field from base class.
  override val value: Amount = 0L
}

object EncryBox {

  type BxTypeId = Byte
}
