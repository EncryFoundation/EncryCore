package encry.modifiers.state.box.proposition

import encry.account.{Account, Address}
import encry.modifiers.Serializer
import encry.modifiers.mempool.Proof
import encry.modifiers.state.box.Context
import encry.settings.Constants
import encry.view.history.Height
import encry.view.state.Proposition
import io.circe.Encoder
import io.circe.syntax._
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.prismlang.compiler.CompiledContract.ContractHash
import org.encryfoundation.prismlang.compiler.{CompiledContract, CostEstimator}
import org.encryfoundation.prismlang.core.wrapped.PValue
import org.encryfoundation.prismlang.core.{Ast, Types}
import org.encryfoundation.prismlang.evaluator.Evaluator
import org.encryfoundation.prismlang.lib.predefined.signature.CheckSig
import scorex.crypto.encode.{Base16, Base58}

import scala.util.{Failure, Success, Try}

case class EncryProposition(contractHash: ContractHash) extends Proposition {

  override type M = EncryProposition

  override def serializer: Serializer[EncryProposition] = EncryPropositionSerializer

  def canUnlock(ctx: Context, contract: CompiledContract, proofs: Seq[Proof]): Boolean =
    if (sameHash(contractHash, contract.hash)) {
      val env: List[(Option[String], PValue)] =
        if (contract.args.isEmpty) List.empty
        else List((None, ctx.transaction.asVal), (None, ctx.state.asVal)) ++ proofs.map(proof => (proof.tagOpt, proof.value))
      val args: List[(String, PValue)] = contract.args.map { case (name, tpe) =>
        env.find(_._1.contains(name)).orElse(env.find(_._2.tpe == tpe)).map(elt => name -> elt._2)
          .getOrElse(throw new Exception("Not enough arguments for contact")) }
      Evaluator.initializedWith(args).eval[Boolean](contract.script)
    } else false

  def isOpen: Boolean = ByteArrayWrapper(contractHash) == ByteArrayWrapper(EncryProposition.open.contractHash)

  def isHeightLockedAt(height: Height): Boolean =
    ByteArrayWrapper(contractHash) == ByteArrayWrapper(EncryProposition.heightLocked(height).contractHash)

  def isLockedByAccount(account: Account): Boolean =
    ByteArrayWrapper(contractHash) == ByteArrayWrapper(EncryProposition.accountLock(account).contractHash)

  def sameHash(h1: Array[Byte], h2: Array[Byte]): Boolean = ByteArrayWrapper(h1) == ByteArrayWrapper(h2)
}

object EncryProposition {

  import org.encryfoundation.prismlang.core.Ast._

  case object UnlockFailedException extends Exception("Unlock failed")

  implicit val jsonEncoder: Encoder[EncryProposition] = (p: EncryProposition) => Map(
    "contractHash" -> Base58.encode(p.contractHash).asJson
  ).asJson

  def openC: CompiledContract = calculateCost(CompiledContract(List.empty, Ast.Expr.True, 0))
  def open: EncryProposition = EncryProposition(openC.hash)

  def heightLockedC(height: Height): CompiledContract = calculateCost({
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
          List(Ast.CompOp.GtE),
          List(Expr.IntConst(height.toLong))
        ),
        Expr.True,
        Expr.False,
        Types.PBoolean
      ), 0
    )
  })
  def heightLocked(height: Height): EncryProposition = EncryProposition(heightLockedC(height).hash)

  def accountLockC(account: Account): CompiledContract = calculateCost({
    CompiledContract(
      List("tx" -> Types.EncryTransaction, "sig" -> Types.Signature25519),
      Expr.Call(
        Expr.Name(Ident("checkSig"), Types.PFunc(CheckSig.args.toList, Types.PBoolean)),
        List(
          Expr.Name(Ident("sig"), Types.Signature25519),
          Expr.Attribute(
            Expr.Name(Ident("tx"), Types.EncryTransaction),
            Ident("messageToSign"),
            Types.PCollection.ofByte
          ),
          Expr.Base16Str(Base16.encode(account.pubKey))
        ),
        Types.PBoolean
      ), 0
    )
  })
  def accountLock(account: Account): EncryProposition = EncryProposition(accountLockC(account).hash)
  def accountLock(address: Address): EncryProposition = accountLock(Account(address))

  def calculateCost(contract: CompiledContract): CompiledContract =
    contract.copy(cost = CostEstimator.default.costOf(contract.script) + contract.args.map(_._2.dataCost).sum)
}

object EncryPropositionSerializer extends Serializer[EncryProposition] {

  override def toBytes(obj: EncryProposition): Array[Byte] = obj.contractHash

  override def parseBytes(bytes: Array[Byte]): Try[EncryProposition] =
    if (bytes.lengthCompare(Constants.DigestLength) == 0) Success(EncryProposition(bytes))
    else Failure(new Exception("Invalid contract hash length"))
}
