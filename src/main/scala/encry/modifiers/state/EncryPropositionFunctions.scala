package encry.modifiers.state

import com.typesafe.scalalogging.StrictLogging
import encry.utils.RegularContractEvaluator
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.modifiers.mempool.transaction.{Proof, PubKeyLockedContract, RegularContract}
import org.encryfoundation.common.modifiers.state.box.EncryProposition
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.prismlang.codec.PCodec
import org.encryfoundation.prismlang.compiler.CompiledContract
import org.encryfoundation.prismlang.core.Ast.Expr
import org.encryfoundation.prismlang.core.wrapped.PValue
import org.encryfoundation.prismlang.evaluator.Evaluator
import scorex.crypto.encode.Base16
import scorex.crypto.hash.Blake2b256
import scorex.crypto.signatures.PublicKey
import scorex.utils.Random

object EncryPropositionFunctions extends StrictLogging {

  val contract = PubKeyLockedContract(PublicKey @@ Random.randomBytes())

  def pubKeyContractBytes(key: Array[Byte]): Array[Byte] = {
    val pubKeyBytes = PCodec.exprCodec.encode(Expr.Base16Str(Base16.encode(key))).require.toByteArray
    contract.contract.bytes.dropRight(pubKeyBytes.length + 1) ++ pubKeyBytes :+ (3: Byte)
  }

  def canUnlock(proposition: EncryProposition, ctx: Context, contract: Either[CompiledContract, RegularContract], proofs: Seq[Proof]): Boolean =
    contract.fold (
      cc => if (sameHash(proposition.contractHash, cc.hash)) {
        val env: List[(Option[String], PValue)] =
          if (cc.args.isEmpty) List.empty
          else List((None, ctx.transaction.asVal), (None, ctx.state.asVal), (None, ctx.box.asVal)) ++
            proofs.map(proof => (proof.tagOpt, proof.value))
        val args: List[(String, PValue)] = cc.args.map { case (name, tpe) =>
          env.find(_._1.contains(name))
            .orElse(env.find(e => e._2.tpe == tpe || tpe.isSubtypeOf(e._2.tpe)))
            .map(elt => name -> elt._2)
            .getOrElse(throw new Exception("Not enough arguments for contact")) }
        Evaluator.initializedWith(args).eval[Boolean](cc.script)
      } else false,
      rc => {
        val contractHash = rc match {
          case PubKeyLockedContract(pubKey) => Blake2b256.hash(pubKeyContractBytes(pubKey))
          case anotherContract => anotherContract.contract.hash
        }
        if (sameHash(proposition.contractHash, contractHash)) RegularContractEvaluator.eval(rc, ctx, proofs)
        else false
      }
  )

  def sameHash(h1: Array[Byte], h2: Array[Byte]): Boolean = ByteArrayWrapper(h1) == ByteArrayWrapper(h2)
}