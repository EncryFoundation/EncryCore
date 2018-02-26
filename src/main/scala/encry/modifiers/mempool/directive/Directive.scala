package encry.modifiers.mempool.directive

import encry.modifiers.mempool.EncryTransaction.Amount
import encry.modifiers.mempool.directive.Directive.DirTypeId
import encry.modifiers.state.box.EncryBaseBox
import scorex.core.serialization.{BytesSerializable, JsonSerializable}

// Directive is sub-modifier of the state.
trait Directive extends BytesSerializable with JsonSerializable {

  val typeId: DirTypeId

  val boxes: Seq[EncryBaseBox]

  val cost: Amount
}

object Directive {

  type DirTypeId = Byte
}
