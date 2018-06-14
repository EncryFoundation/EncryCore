package encry.modifiers.state.box.proposition

import encry.modifiers.state.box.Context
import encry.modifiers.state.box.proof.Proof
import encry.settings.Algos
import encry.view.history.Height
import io.circe.Encoder
import io.circe.syntax._
import org.encryfoundation.prismlang.compiler.{CompiledContract, CompiledContractSerializer}
import org.encryfoundation.prismlang.core.wrapped.{PObject, PValue}
import org.encryfoundation.prismlang.core.{Ast, PConvertible, Types}
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

  import org.encryfoundation.prismlang.core.Ast._

  case object UnlockFailedException extends Exception("Unlock failed")

  implicit val jsonEncoder: Encoder[EncryProposition] = (p: EncryProposition) => Map(
    "script" -> Base58.encode(p.contract.bytes).asJson
  ).asJson

  def open: EncryProposition = EncryProposition(CompiledContract(List.empty, Ast.Expr.True))

  def heightLocked(height: Height): EncryProposition = EncryProposition(
    CompiledContract(
      List("state" -> Types.EncryState),
      Expr.If(
        Expr.Compare(
          Expr.Attribute(
            Expr.Name(
              Ast.Ident("state"),
              Types.EncryState
            ),
            Ast.Ident("height"),
            Types.PInt
          ),
          List(Ast.CompOp.Gt),
          List(Expr.IntConst(height))
        ),
        Expr.True,
        Expr.False,
        Types.PBoolean
      )
    )
  )
}

object EncryPropositionSerializer extends Serializer[EncryProposition] {

  override def toBytes(obj: EncryProposition): Array[Byte] = obj.contract.bytes

  override def parseBytes(bytes: Array[Byte]): Try[EncryProposition] = CompiledContractSerializer.parseBytes(bytes)
    .map { contract => EncryProposition(contract) }
}
