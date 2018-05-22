package encry.modifiers.state.box.proposition

import com.google.common.primitives.{Bytes, Ints}
import encry.contracts.{CStateInfo, ContractContext}
import encry.modifiers.state.box.Context
import encry.modifiers.state.box.proof.Proof
import encry.modifiers.state.box.proposition.EncryProposition.UnlockFailedException
import encry.settings.Constants
import encrywm.common.SourceProcessor.SerializedContract
import encrywm.common.{EncryContract, ScriptFingerprint, ScriptMeta, ScriptSerializer}
import encrywm.lang.backend.env.{ESObject, ESValue}
import encrywm.lang.backend.executor.Executor
import encrywm.lang.backend.executor.Executor.{Return, Unlocked}
import encrywm.lang.frontend.semantics.ComplexityAnalyzer.ScriptComplexityScore
import encrywm.lib.Types
import encrywm.lib.Types.{ESByteVector, ESInt}
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
        case _ => throw UnlockFailedException
      }
    }

  override val esType: Types.ESProduct = Types.ContractProposition

  override def asVal: ESValue = ESValue(Types.ContractProposition.ident.toLowerCase, Types.ContractProposition)(convert)

  override def convert: ESObject = {
    val fields = Map(
      "typeId" -> ESValue("typeId", ESInt)(typeId.toInt),
      "fingerprint" -> ESValue("fingerprint", ESByteVector)(contract.meta.scriptFingerprint)
    )
    ESObject(Types.ContractProposition.ident, fields, esType)
  }
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
