package encry.modifiers.state.box.proof

import encry.modifiers.state.box.proof.Proof.ProofTypeId
import encrywm.lib.predef.env.ESEnvConvertable
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor}
import scorex.core.serialization.BytesSerializable

trait Proof extends ESEnvConvertable with BytesSerializable {

  val typeId: ProofTypeId
}

object Proof {

  type ProofTypeId = Byte

  implicit val jsonEncoder: Encoder[Proof] = {
    case s25519: Signature25519 =>
      Signature25519.jsonEncoder(s25519)
  }

  implicit val jsonDecoder: Decoder[Option[Proof]] = (c: HCursor) => {
    Decoder.decodeOption[Proof](decoder)(c).fold(_ => Right(None), pr => Right(pr))
  }

  implicit val decoder: Decoder[Proof] = Decoder.instance { c =>
    Decoder.decodeByte.tryDecode(c.downField("typeId")) match {
      case Right(s) => s match {
        case Signature25519.TypeId => Signature25519.jsonDecoder(c)
        case _ => Left(DecodingFailure("Incorrect proof type", c.history))
      }
      case Left(_) => Left(DecodingFailure("None typeId", c.history))
    }
  }
}
