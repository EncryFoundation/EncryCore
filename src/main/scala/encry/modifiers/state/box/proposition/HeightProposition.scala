package encry.modifiers.state.box.proposition

import com.google.common.primitives.Ints
import encry.modifiers.state.box.Context
import encry.modifiers.state.box.proof.Proof
import encry.modifiers.state.box.serializers.SizedCompanionSerializer
import encry.view.history.Height
import scorex.core.serialization.Serializer

import scala.util.{Failure, Success, Try}

case class HeightProposition(height: Height) extends EncryProposition {

  override type M = HeightProposition

  override def serializer: Serializer[M] = HeightPropositionSerializer

  override def unlockTry(proof: Proof)(implicit ctx: Context): Try[Unit] =
    if (height <= ctx.height) Success()
    else Failure(new Error("Unlock failed"))
}

object HeightPropositionSerializer extends SizedCompanionSerializer[HeightProposition] {

  val Size = 3

  override def toBytes(obj: HeightProposition): Array[Byte] = Ints.toByteArray(obj.height)

  override def parseBytes(bytes: Array[Byte]): Try[HeightProposition] = Try {
    HeightProposition(Height @@ Ints.fromByteArray(bytes))
  }
}
