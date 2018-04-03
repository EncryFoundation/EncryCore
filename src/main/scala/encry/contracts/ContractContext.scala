package encry.contracts

import encry.modifiers.mempool.EncryBaseTransaction
import encry.modifiers.state.box.proof.Proof
import encry.view.history.Height
import encrywm.backend.env.{ESObject, ESValue}
import encrywm.core.Types._
import encrywm.core.predef.env.ESEnvConvertable
import scorex.crypto.authds.ADDigest

case class CStateInfo(height: Height, lastBlockTimestamp: Long, stateDigest: ADDigest) extends ESEnvConvertable {

  override val esType: ESProduct = ESState

  override def asVal: ESValue = ESValue("state", ESState)(convert)

  override def convert: ESObject = {
    val fields = Map(
      "height" -> ESValue("height", ESLong)(height),
      "lastBlockTimestamp" -> ESValue("lastBlockTimestamp", ESLong)(lastBlockTimestamp),
      "stateDigest" -> ESValue("stateDigest", ESByteVector)(stateDigest)
    )
    ESObject(ESState.ident, fields, esType)
  }
}

class ContractContext(proof: Proof,
                      transaction: EncryBaseTransaction,
                      si: CStateInfo) extends ESEnvConvertable {

  override val esType: ESProduct = ESContext

  override def asVal: ESValue = ESValue("context", ESContext)(convert)

  override def convert: ESObject = {
    val fields = Map(
      "proof" -> proof.asVal,
      "transaction" -> transaction.asVal,
      "state" -> si.asVal
    )
    ESObject(ESContext.ident, fields, esType)
  }
}
