package encry.modifiers.state.box.proposition

import com.google.common.primitives.{Bytes, Ints}
import encry.contracts.{CStateInfo, ContractContext}
import encry.modifiers.state.box.Context
import encry.modifiers.state.box.proof.Proof
import encry.settings.Constants
import encrywm.backend.env.{ESObject, ESValue}
import encrywm.backend.executor.Executor
import encrywm.backend.executor.Executor.{Return, Unlocked}
import encrywm.common.SourceProcessor.SerializedContract
import encrywm.common.{EncryContract, ScriptFingerprint, ScriptMeta, ScriptSerializer}
import encrywm.frontend.semantics.ComplexityAnalyzer.ScriptComplexityScore
import encrywm.lib.Types
import encrywm.lib.Types.ESProposition
import io.circe.Encoder
import io.circe.syntax._
import scorex.core.serialization.Serializer
import scorex.crypto.encode.Base58

import scala.util.{Success, Try}

case class ContractProposition(contract: EncryContract) extends EncryProposition {

  override type M = ContractProposition

  override val typeId: Byte = ContractProposition.TypeId

  override def serializer: Serializer[M] = ContractPropositionSerializer

  override def unlockTry(proof: Proof)(implicit ctx: Context): Try[Unit] =
    ScriptSerializer.deserialize(contract.serializedScript).map { script =>
      val contractContext = ContractContext(proof, ctx.transaction,
        CStateInfo(ctx.height, ctx.lastBlockTimestamp, ctx.stateDigest), contract.meta)
      val executor = Executor(contractContext.asVal, Constants.ContractMaxFuel)
      executor.executeContract(script) match {
        case Right(Return(_: Unlocked.type)) => Success()
        case _ => throw new Error("Unlock failed.")
      }
    }

  override val esType: Types.ESProduct = ESProposition

  override def asVal: ESValue = ???

  override def convert: ESObject = ???
}

object ContractProposition {

  val TypeId: Byte = 1

  implicit val jsonEncoder: Encoder[ContractProposition] = (p: ContractProposition) => Map(
    "typeId" -> TypeId.toInt.asJson,
    "script" -> Base58.encode(p.contract.serializedScript).asJson
  ).asJson

  def apply(sc: SerializedContract, scs: ScriptComplexityScore, sf: ScriptFingerprint): ContractProposition =
    ContractProposition(EncryContract(sc, ScriptMeta(scs, sf)))
}

object ContractPropositionSerializer extends Serializer[ContractProposition] {

  override def toBytes(obj: ContractProposition): Array[Byte] = Bytes.concat(
      obj.contract.serializedScript,
      Ints.toByteArray(obj.contract.meta.complexityScore),
      obj.contract.meta.scriptFingerprint
    )

  override def parseBytes(bytes: Array[Byte]): Try[ContractProposition] = Try {
    val complexity = Ints.fromByteArray(bytes.dropRight(8).takeRight(4))
    val fingerprint = bytes.takeRight(8)
    ContractProposition(EncryContract(bytes.dropRight(12), ScriptMeta(complexity, fingerprint)))
  }
}
