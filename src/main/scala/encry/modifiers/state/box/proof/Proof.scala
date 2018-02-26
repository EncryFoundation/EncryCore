package encry.modifiers.state.box.proof

import encry.modifiers.state.box.proof.Proof.ProofTypeId
import scorex.core.serialization.{BytesSerializable, JsonSerializable}

trait Proof extends BytesSerializable with JsonSerializable {

  val typeId: ProofTypeId
}

object Proof {

  type ProofTypeId = Byte
}
