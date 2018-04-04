package encry.modifiers.state.box.proof

import encry.modifiers.state.box.proof.Proof.ProofTypeId
import encrywm.lib.predef.env.ESEnvConvertable
import io.circe.Encoder
import scorex.core.serialization.BytesSerializable

trait Proof extends ESEnvConvertable with BytesSerializable {

  val typeId: ProofTypeId
}

object Proof {

  type ProofTypeId = Byte

  implicit val jsonEncoder: Encoder[Proof] = {
    case s25519: Signature25519 => Signature25519.jsonEncoder(s25519)
  }
}
