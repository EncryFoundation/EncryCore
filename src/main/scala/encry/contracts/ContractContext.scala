package encry.contracts

import encry.modifiers.mempool.EncryBaseTransaction
import encry.modifiers.state.box.proof.Proof
import encry.view.history.Height
import encrywm.backend.env.{ESObject, ESValue}
import encrywm.common.ScriptMeta
import encrywm.lib.Types._
import encrywm.lib.predef.env.ESEnvConvertable
import scorex.crypto.authds.ADDigest

case class CStateInfo(height: Height, lastBlockTimestamp: Long, stateDigest: ADDigest) extends ESEnvConvertable {

  override val esType: ESProduct = ESState

  override def asVal: ESValue = ESValue(ESState.ident.toLowerCase, ESState)(convert)

  override def convert: ESObject = {
    val fields = Map(
      "height" -> ESValue("height", ESLong)(height),
      "lastBlockTimestamp" -> ESValue("lastBlockTimestamp", ESLong)(lastBlockTimestamp),
      "stateDigest" -> ESValue("stateDigest", ESByteVector)(stateDigest)
    )
    ESObject(ESState.ident, fields, esType)
  }
}

case class ContractContext(proof: Proof,
                           transaction: EncryBaseTransaction,
                           si: CStateInfo,
                           selfMeta: ScriptMeta) extends ESEnvConvertable {

  override val esType: ESProduct = ESContext

  override def asVal: ESValue = ESValue(ESContext.ident.toLowerCase, ESContext)(convert)

  override def convert: ESObject = {
    val fields = Map(
      "proof" -> proof.asVal,
      "transaction" -> transaction.asVal,
      "state" -> si.asVal,
      "self" -> selfMeta.asVal
    )
    ESObject(ESContext.ident, fields, esType)
  }
}
