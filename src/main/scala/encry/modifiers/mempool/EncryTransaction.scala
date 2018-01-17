package encry.modifiers.mempool

import com.google.common.primitives.Longs
import scorex.core.serialization.JsonSerializable
import scorex.core.utils.ScorexLogging

trait EncryTransaction extends EncryBaseTransaction with JsonSerializable with ScorexLogging

object EncryTransaction {

  type TxTypeId = Byte
  type Nonce = Long
  type Amount = Long

  def nonceFromDigest(digest: Array[Byte]): Nonce = Longs.fromByteArray(digest.take(8))
}
