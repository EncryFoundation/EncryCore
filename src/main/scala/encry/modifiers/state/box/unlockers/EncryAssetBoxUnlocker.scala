package encry.modifiers.state.box.unlockers

import scorex.core.transaction.box.BoxUnlocker
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.crypto.authds.ADKey

case class EncryAssetBoxUnlocker(override val closedBoxId: ADKey,
                                 override val boxKey: Signature25519)
  extends BoxUnlocker[PublicKey25519Proposition]
