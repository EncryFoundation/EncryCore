package encry.modifiers.mempool

import com.google.common.primitives.Longs
import scorex.core.serialization.JsonSerializable
import scorex.core.transaction.box.BoxUnlocker
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.utils.ScorexLogging

trait EncryTransaction[P <: Proposition]
  extends EncryBaseTransaction with JsonSerializable with ScorexLogging{

  val proposition: P

  val unlockers: Traversable[BoxUnlocker[P]]
}

object EncryTransaction {

  type TxTypeId = Byte
  type Nonce = Long
  type Amount = Long

  def nonceFromDigest(digest: Array[Byte]): Nonce = Longs.fromByteArray(digest.take(8))
}
