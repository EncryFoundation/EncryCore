package encry.modifiers.state.box.proposition

import encry.modifiers.state.box.Context
import encry.modifiers.state.box.proof.Proof
import encry.settings.Algos
import io.circe.Encoder
import io.circe.syntax._
import org.encryfoundation.prismlang.compiler.{CompiledContract, CompiledContractSerializer}
import org.encryfoundation.prismlang.core.wrapped.{PObject, PValue}
import org.encryfoundation.prismlang.core.{PConvertible, Types}
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.Proposition
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Digest32

import scala.util.Try

case class EncryProposition(contract: CompiledContract) extends Proposition with PConvertible {

  override type M = EncryProposition

  override def serializer: Serializer[EncryProposition] = EncryPropositionSerializer

  override val esType: Types.Product = Types.EncryProposition

  override def asVal: PValue = PValue(esType)(convert)

  override def convert: PObject = ???

  def unlockTry(proof: Proof, ctx: Context): Try[Unit] = ???

  lazy val contractHash: Digest32 = Algos.hash(contract.bytes)
}

object EncryProposition {

  case object UnlockFailedException extends Exception("Unlock failed")

  implicit val jsonEncoder: Encoder[EncryProposition] = (p: EncryProposition) => Map(
    "script" -> Base58.encode(p.contract.bytes).asJson
  ).asJson
}

object EncryPropositionSerializer extends Serializer[EncryProposition] {

  override def toBytes(obj: EncryProposition): Array[Byte] = obj.contract.bytes

  override def parseBytes(bytes: Array[Byte]): Try[EncryProposition] = CompiledContractSerializer.parseBytes(bytes)
    .map { contract => EncryProposition(contract) }
}
