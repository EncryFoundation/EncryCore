package encry.modifiers.mempool

import scorex.core.serialization.BytesSerializable
import scorex.core.transaction.box.proposition.Proposition
import scorex.crypto.authds.ADKey

trait EncryBaseBox[P <: Proposition, B <: BaseBoxBody] extends AnyRef with BytesSerializable{
  val proposition: P
  val id: ADKey
  val body: B
}

object EncryBaseBox extends AnyRef with Serializable