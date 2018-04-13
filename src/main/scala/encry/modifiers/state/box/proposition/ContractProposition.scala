package encry.modifiers.state.box.proposition

import com.google.common.primitives.{Bytes, Ints}
import encry.contracts.{CStateInfo, ContractContext}
import encry.modifiers.state.box.Context
import encry.modifiers.state.box.proof.Proof
import encrywm.backend.env.ScopedRuntimeEnv
import encrywm.backend.executor.Executor
import encrywm.backend.executor.Executor.{Return, Unlocked}
import encrywm.common.SourceProcessor.SerializedContract
import encrywm.common.{ESContract, ScriptFingerprint, ScriptMeta, ScriptSerializer}
import encrywm.frontend.semantics.ComplexityAnalyzer.ScriptComplexityScore
import io.circe.Encoder
import io.circe.syntax._
import scorex.core.serialization.Serializer
import scorex.crypto.encode.Base58

import scala.util.{Success, Try}

case class ContractProposition(contract: ESContract) extends EncryProposition {

  override type M = ContractProposition

  override def serializer: Serializer[M] = ContractPropositionSerializer

  override def unlockTry(proof: Proof)(implicit ctx: Context): Try[Unit] =
    ScriptSerializer.deserialize(contract.serializedScript).map { script =>
      val contractContext = new ContractContext(proof, ctx.transaction, CStateInfo(ctx.height, ctx.lastBlockTimestamp, ctx.stateDigest))
      val executor = new Executor(ScopedRuntimeEnv.initialized("global", 1, Map("context" -> contractContext.asVal)))
      executor.executeContract(script) match {
        case Right(Return(_: Unlocked.type)) => Success()
        case _ => throw new Error("Unlock failed.")
      }
    }
}

object ContractProposition {

  val TypeId: Byte = 1

  implicit val jsonEncoder: Encoder[ContractProposition] = (p: ContractProposition) => Map(
    "typeId" -> TypeId.toInt.asJson,
    "script" -> Base58.encode(p.contract.serializedScript).asJson
  ).asJson

  def apply(sc: SerializedContract, scs: ScriptComplexityScore, sf: ScriptFingerprint): ContractProposition =
    ContractProposition(ESContract(sc, ScriptMeta(scs, sf)))
}

object ContractPropositionSerializer extends Serializer[ContractProposition] {

  // TODO: Move contract serialization logic to EncryScript library.
  override def toBytes(obj: ContractProposition): Array[Byte] = Bytes.concat(
      obj.contract.serializedScript,
      Ints.toByteArray(obj.contract.meta.complexityScore),
      obj.contract.meta.scriptFingerprint
    )

  override def parseBytes(bytes: Array[Byte]): Try[ContractProposition] = Try {
    val complexity = Ints.fromByteArray(bytes.dropRight(8).takeRight(4))
    val fingerprint = bytes.takeRight(8)
    ContractProposition(ESContract(bytes.dropRight(12), ScriptMeta(complexity, fingerprint)))
  }
}
