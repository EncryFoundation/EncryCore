package encry.modifiers.state

import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.modifiers.mempool.transaction.Proof
import org.encryfoundation.common.modifiers.state.box.EncryProposition
import org.encryfoundation.prismlang.compiler.CompiledContract
import org.encryfoundation.prismlang.core.wrapped.PValue
import org.encryfoundation.prismlang.evaluator.Evaluator

object EncryPropositionFunctions {

  def canUnlock(proposition: EncryProposition, ctx: Context, contract: CompiledContract, proofs: Seq[Proof]): Boolean =
    if (sameHash(proposition.contractHash, contract.hash)) {
      val env: List[(Option[String], PValue)] =
        if (contract.args.isEmpty) List.empty
        else List((None, ctx.transaction.asVal), (None, ctx.state.asVal), (None, ctx.box.asVal)) ++
          proofs.map(proof => (proof.tagOpt, proof.value))
      val args: List[(String, PValue)] = contract.args.map { case (name, tpe) =>
        env.find(_._1.contains(name))
          .orElse(env.find(e => e._2.tpe == tpe || tpe.isSubtypeOf(e._2.tpe)))
          .map(elt => name -> elt._2)
          .getOrElse(throw new Exception("Not enough arguments for contact")) }
      Evaluator.initializedWith(args).eval[Boolean](contract.script)
    } else false

  def sameHash(h1: Array[Byte], h2: Array[Byte]): Boolean = ByteArrayWrapper(h1) == ByteArrayWrapper(h2)
}