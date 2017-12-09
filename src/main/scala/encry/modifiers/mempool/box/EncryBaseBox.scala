package encry.modifiers.mempool.box

import encry.modifiers.mempool.box.body.BaseBoxBody
import scorex.core.serialization.{BytesSerializable, JsonSerializable}
import scorex.core.transaction.box.proposition.Proposition
import scorex.crypto.authds.ADKey

trait EncryBaseBox[P <: Proposition, BB <: BaseBoxBody] extends BytesSerializable with JsonSerializable {
  val proposition: P
  val id: ADKey
  val body: BB
}

object EncryBaseBox extends Serializable