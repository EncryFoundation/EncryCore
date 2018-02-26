package encry.modifiers.mempool.directive

import encry.modifiers.mempool.directive.Directive.DirTypeId
import encry.modifiers.state.box.EncryBaseBox
import scorex.core.serialization.{BytesSerializable, JsonSerializable}
import scorex.core.transaction.box.Box.Amount
import scorex.crypto.hash.Digest32

// Directive is sub-modifier of the state.
trait Directive extends BytesSerializable with JsonSerializable {

  val typeId: DirTypeId

  val hash: Digest32

  val idx: Int

  val cost: Amount

  val isValid: Boolean

  def boxes(digest: Digest32): Seq[EncryBaseBox]
}

object Directive {

  type DirTypeId = Byte
}
