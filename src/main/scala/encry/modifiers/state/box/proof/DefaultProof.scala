package encry.modifiers.state.box.proof
import encry.modifiers.state.box.proof.Proof.ProofTypeId
import io.circe.Json
import scorex.core.serialization.Serializer

import scala.util.Try

object DefaultProof extends Proof {

  override type M = this.type

  override val typeId: ProofTypeId = 0.toByte

  override def serializer: Serializer[M] = ???

  override def json: Json = ???
}

object DefaultProofSerializer extends Serializer[DefaultProof.type] {

  override def toBytes(obj: DefaultProof.type): Array[Byte] = Array(obj.typeId)

  override def parseBytes(bytes: Array[Byte]): Try[DefaultProof.type] = ???
}
