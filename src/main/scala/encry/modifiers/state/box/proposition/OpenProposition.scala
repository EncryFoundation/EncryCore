package encry.modifiers.state.box.proposition

import encry.modifiers.state.box.Context
import encry.modifiers.state.box.proof.Proof
import encrywm.lang.backend.env.{ESObject, ESValue}
import encrywm.lib.Types
import encrywm.lib.Types.ESInt
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.ProofOfKnowledgeProposition

import scala.util.{Failure, Success, Try}

object OpenProposition extends ProofOfKnowledgeProposition[Nothing] with EncryProposition {

  override type M = OpenProposition.type

  override val typeId: Byte = 0

  override lazy val serializer: Serializer[OpenProposition.type] = OpenPropositionSerializer

  override def unlockTry(proof: Proof)(implicit ctx: Context): Try[Unit] = Success()

  override val esType: Types.ESProduct = Types.OpenProposition

  override def asVal: ESValue = ESValue(Types.OpenProposition.ident.toLowerCase, Types.OpenProposition)(convert)

  override def convert: ESObject = {
    val fields = Map(
      "typeId" -> ESValue("typeId", ESInt)(typeId.toInt)
    )
    ESObject(Types.OpenProposition.ident, fields, esType)
  }
}

object OpenPropositionSerializer extends Serializer[OpenProposition.type] {

  val Length = 1
  val ByteValue: Array[Byte] = Array.fill(Length)(127: Byte)

  override def toBytes(obj: OpenProposition.type): Array[Byte] = ByteValue

  override def parseBytes(bytes: Array[Byte]): Try[OpenProposition.type] = bytes match {
    case b if b sameElements ByteValue => Success(OpenProposition)
    case l => Failure(new Exception(s"Incorrect proposition ${l.headOption}"))
  }
}
