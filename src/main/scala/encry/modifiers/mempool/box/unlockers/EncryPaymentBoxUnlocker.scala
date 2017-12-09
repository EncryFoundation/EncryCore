package encry.modifiers.mempool.box.unlockers

import scorex.core.transaction.box.BoxUnlocker
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Proof
import scorex.crypto.authds.ADKey

case class EncryPaymentBoxUnlocker(override val closedBoxId: ADKey,
                                   override val boxKey: Proof[PublicKey25519Proposition])
  extends BoxUnlocker[PublicKey25519Proposition]
