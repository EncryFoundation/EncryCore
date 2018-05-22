package encry.modifiers.state.box.proposition

import encry.modifiers.state.box.Context
import encry.modifiers.state.box.proof.Proof
import encrywm.lang.backend.env.ESEnvConvertable
import io.circe.Encoder
import io.circe.syntax._
import scorex.core.transaction.box.proposition.Proposition

import scala.util.Try

trait EncryProposition extends Proposition with ESEnvConvertable {

  val typeId: Byte

  def unlockTry(proof: Proof)(implicit ctx: Context): Try[Unit]
}

object EncryProposition {

  case object UnlockFailedException extends Exception("Unlock failed")

  implicit val jsonEncoder: Encoder[EncryProposition] = {
    case _: OpenProposition.type => "OpenProposition".asJson
    case ap: AccountProposition => AccountProposition.jsonEncoder(ap)
    case cp: ContractProposition => ContractProposition.jsonEncoder(cp)
    case hp: HeightProposition => HeightProposition.jsonEncoder(hp)
  }
}
