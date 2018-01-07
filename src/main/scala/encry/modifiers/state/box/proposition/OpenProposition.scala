package encry.modifiers.state.box.proposition

import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.ProofOfKnowledgeProposition

import scala.util.{Failure, Success, Try}

object OpenProposition extends ProofOfKnowledgeProposition[Nothing] {

  override type M = OpenProposition.type

  override lazy val serializer: Serializer[OpenProposition.type] = OpenPropositionSerializer
}

object OpenPropositionSerializer extends Serializer[OpenProposition.type] {

  val Length = 1
  val ByteValue: Array[Byte] = Array.fill(Length)(-127: Byte)

  override def toBytes(obj: OpenProposition.type): Array[Byte] = ByteValue

  override def parseBytes(bytes: Array[Byte]): Try[OpenProposition.type] = bytes match {
    case b if b sameElements ByteValue => Success(OpenProposition)
    case l => Failure(new Error(s"Incorrect proposition ${l.headOption}"))
  }
}
