package encry.modifiers.state.box.proposition

import com.google.common.primitives.Ints
import encry.contracts.{CStateInfo, ContractContext}
import encry.modifiers.state.box.Context
import encry.modifiers.state.box.proof.Proof
import encrywm.backend.env.ScopedRuntimeEnv
import encrywm.backend.executor.Executor
import encrywm.common.ScriptPreprocessor.SerializedContract
import encrywm.common.{ESContract, ScriptMeta, ScriptSerializer}
import encrywm.frontend.semantics.ComplexityAnalyzer.ScriptComplexityScore
import scorex.core.serialization.Serializer

import scala.util.Try

case class ContractProposition(contract: ESContract) extends EncryProposition {

  override type M = ContractProposition

  override def serializer: Serializer[M] = ContractPropositionSerializer

  override def unlockTry(proof: Proof)(implicit ctx: Context): Try[Unit] = Try {
    val contractDeserialized = ScriptSerializer.deserialize(contract.serializedScript).get
    val contractContext = new ContractContext(proof, ctx.transaction, CStateInfo(ctx.height, 1234567L, ctx.stateDigest))  // TODO: Use real timestamp when field is added to state meta.
    val executor = new Executor(ScopedRuntimeEnv.initialized("global", 1, Map("context" -> contractContext.asVal)))
    if (!executor.executeContract(contractDeserialized).right.get.r.isInstanceOf[Executor.Unlocked.type])
      throw new Error("Unlock failed.")
  }
}

object ContractProposition {

  val TypeId: Byte = 1

  def apply(sc: SerializedContract, scs: ScriptComplexityScore): ContractProposition =
    ContractProposition(ESContract(sc, ScriptMeta(scs)))
}

object ContractPropositionSerializer extends Serializer[ContractProposition] {

  // TODO: Move contract serialization logic to EncryScript library.
  override def toBytes(obj: ContractProposition): Array[Byte] =
    obj.contract.serializedScript ++ Ints.toByteArray(obj.contract.meta.complexityScore)

  override def parseBytes(bytes: Array[Byte]): Try[ContractProposition] = Try {
    val complexity = Ints.fromByteArray(bytes.takeRight(4))
    ContractProposition(ESContract(bytes.dropRight(4), ScriptMeta(complexity)))
  }
}
