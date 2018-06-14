package encry.modifiers.state.box.proposition

import encry.modifiers.state.box.Context
import encry.modifiers.state.box.proof.Proof
import encry.settings.Algos
import encry.view.history.Height
import io.circe.Encoder
import io.circe.syntax._
import org.encryfoundation.prismlang.compiler.{CompiledContract, CompiledContractSerializer}
import org.encryfoundation.prismlang.core.wrapped.{PObject, PValue}
import org.encryfoundation.prismlang.core.{Ast, PConvertible, TypeSystem, Types}
import org.encryfoundation.prismlang.evaluator.{Evaluator, ScopedRuntimeEnvironment}
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

  override def convert: PObject = PObject(Map("fingerprint" -> PValue(Types.PCollection.ofByte)(contractHash)), esType)

  def canUnlock(ctx: Context, proofs: Seq[Proof], namedProofs: Seq[(String, Proof)]): Boolean = {
    val env: List[(String, Types.Product, PObject)] = if (contract.args.isEmpty) List.empty else {
      List(("*", ctx.transaction.esType, ctx.transaction.convert), ("*", ctx.state.esType, ctx.state.convert)) ++
        proofs.map(proof => ("*", proof.esType, proof.convert)) ++
        namedProofs.map { case (name, proof) => (name, proof.esType, proof.convert) }
    }
    val args: List[(String, PObject)] = contract.args.map { case (name, tpe) =>
      env.find(_._1 == name).orElse(env.find(_._2 == tpe)).map(elt => elt._1 -> elt._3)
        .getOrElse(throw new Exception("Not enough arguments for contact"))
    }
    val evaluator: Evaluator = Evaluator(ScopedRuntimeEnvironment.initialized(1, args.toMap), TypeSystem.default)
    evaluator.eval[Boolean](contract.script)
  }

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
    .map(EncryProposition.apply)
}
