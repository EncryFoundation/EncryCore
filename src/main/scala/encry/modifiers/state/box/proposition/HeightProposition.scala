package encry.modifiers.state.box.proposition

import com.google.common.primitives.Longs
import encry.modifiers.state.box.serializers.SizedCompanionSerializer
import encry.view.history.Height
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.Proposition

import scala.util.Try

case class HeightProposition(height: Height) extends Proposition {

  override type M = HeightProposition

  override def serializer: Serializer[M] = HeightPropositionSerializer
}

object HeightPropositionSerializer extends SizedCompanionSerializer[HeightProposition] {

  val Size = 8

  override def toBytes(obj: HeightProposition): Array[Byte] = Longs.toByteArray(obj.height)

  override def parseBytes(bytes: Array[Byte]): Try[HeightProposition] = Try {
    HeightProposition(Height @@ Longs.fromByteArray(bytes))
  }
}
