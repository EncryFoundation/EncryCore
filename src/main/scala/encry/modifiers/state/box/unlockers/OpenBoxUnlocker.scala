package encry.modifiers.state.box.unlockers

import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.crypto.authds.ADKey

case class OpenBoxUnlocker(override val closedBoxId: ADKey,
                           override val boxKey: Signature25519)
  extends EncryBoxUnlocker[PublicKey25519Proposition] {

  def isValid(txProp: PublicKey25519Proposition, msg: Array[Byte]): Boolean =
    boxKey.isValid(txProp, msg)
}
