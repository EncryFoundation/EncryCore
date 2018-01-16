package encry.modifiers.state.box.unlockers

import encry.modifiers.state.box.proposition.AddressProposition
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.crypto.authds.ADKey

case class AssetBoxUnlocker(override val closedBoxId: ADKey,
                            override val boxKey: Signature25519)
  extends EncryBoxUnlocker[PublicKey25519Proposition] {

  def isValid(bxProp: AddressProposition, txProp: PublicKey25519Proposition, msg: Array[Byte]): Boolean =
    boxKey.isValid(txProp, msg) && txProp.address == bxProp.address
}
