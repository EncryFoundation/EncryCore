package encry.modifiers.state.box.proposition

import encry.modifiers.state.box.Context
import encry.modifiers.state.box.proof.Proof
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.ProofOfKnowledgeProposition

import scala.util.{Failure, Success, Try}

object OpenProposition extends ProofOfKnowledgeProposition[Nothing] with EncryProposition {

  override type M = OpenProposition.type

  override lazy val serializer: Serializer[OpenProposition.type] = OpenPropositionSerializer

  override def unlockTry(proof: Proof)(implicit ctx: Context): Try[Unit] = Success()
}

object OpenPropositionSerializer extends Serializer[OpenProposition.type] {

  val TypeId: Byte = 0

  val Length = 1
  val ByteValue: Array[Byte] = Array.fill(Length)(-127: Byte)

  override def toBytes(obj: OpenProposition.type): Array[Byte] = ByteValue

  override def parseBytes(bytes: Array[Byte]): Try[OpenProposition.type] = bytes match {
    case b if b sameElements ByteValue => Success(OpenProposition)
    case l => Failure(new Error(s"Incorrect proposition ${l.headOption}"))
  }
}
