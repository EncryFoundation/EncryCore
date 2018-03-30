package encry.modifiers.state.box.proposition

import com.google.common.primitives.Ints
import encrywm.common.ScriptPreprocessor.SerializedContract
import encrywm.common.{ESContract, ScriptMeta}
import encrywm.frontend.semantics.ComplexityAnalyzer.ScriptComplexityScore
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.Proposition

import scala.util.Try

case class ContractProposition(contract: ESContract) extends Proposition {

  override type M = ContractProposition

  override def serializer: Serializer[M] = ContractPropositionSerializer
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
