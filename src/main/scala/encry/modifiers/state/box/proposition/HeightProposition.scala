package encry.modifiers.state.box.proposition

import com.google.common.primitives.Ints
import encry.modifiers.state.box.Context
import encry.modifiers.state.box.proof.Proof
import encry.modifiers.state.box.proposition.EncryProposition.UnlockFailedException
import encry.view.history.Height
import encrywm.lang.backend.env.{ESObject, ESValue}
import encrywm.lib.Types
import encrywm.lib.Types.ESInt
import io.circe.Encoder
import io.circe.syntax._
import scorex.core.serialization.Serializer

import scala.util.{Failure, Success, Try}

case class HeightProposition(height: Height) extends EncryProposition {

  override type M = HeightProposition

  override val typeId: Byte = HeightProposition.TypeId

  override def serializer: Serializer[M] = HeightPropositionSerializer

  override def unlockTry(proof: Proof)(implicit ctx: Context): Try[Unit] =
    if (height <= ctx.height) Success()
    else Failure(UnlockFailedException)

  override val esType: Types.ESProduct = Types.HeightProposition

  override def asVal: ESValue = ESValue(Types.HeightProposition.ident.toLowerCase, Types.HeightProposition)(convert)

  override def convert: ESObject = {
    val fields = Map(
      "typeId" -> ESValue("typeId", ESInt)(typeId.toInt),
      "height" -> ESValue("height", ESInt)(height)
    )
    ESObject(Types.HeightProposition.ident, fields, esType)
  }
}

object HeightProposition {

  val TypeId: Byte = 3.toByte

  implicit val jsonEncoder: Encoder[HeightProposition] = (p: HeightProposition) => Map(
    "typeId" -> TypeId.toInt.asJson,
    "height" -> p.height.toInt.asJson
  ).asJson
}

object HeightPropositionSerializer extends Serializer[HeightProposition] {

  val Size = 4

  override def toBytes(obj: HeightProposition): Array[Byte] = Ints.toByteArray(obj.height)

  override def parseBytes(bytes: Array[Byte]): Try[HeightProposition] = Try {
    HeightProposition(Height @@ Ints.fromByteArray(bytes))
  }
}
