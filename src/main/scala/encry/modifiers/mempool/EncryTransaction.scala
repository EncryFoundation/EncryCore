package encry.modifiers.mempool

import com.google.common.primitives.Longs
import encry.modifiers.state.box.EncryBaseBox
import scorex.core.serialization.JsonSerializable
import scorex.core.transaction.box.BoxUnlocker
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.utils.ScorexLogging

trait EncryTransaction[P <: Proposition]
  extends EncryBaseTransaction with JsonSerializable with ScorexLogging{

  // `BoxUnlocker` holds ID and Key of the box to open (Sequence of `Tx Inputs` + Keys to unlock them).
  val unlockers: Traversable[BoxUnlocker[P]]
  // Sequence of `Tx Outputs`.
  val newBoxes: Traversable[EncryBaseBox]
}

object EncryTransaction {

  type TxTypeId = Byte
  type Nonce = Long
  type Amount = Long

  def nonceFromDigest(digest: Array[Byte]): Nonce = Longs.fromByteArray(digest.take(8))
}
