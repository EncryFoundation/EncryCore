package encry.modifiers.mempool

import com.google.common.primitives.{Bytes, Longs}
import encry.modifiers.state.box.EncryBaseBox
import encry.modifiers.state.box.body.BaseBoxBody
import scorex.core.serialization.JsonSerializable
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.BoxUnlocker
import scorex.core.transaction.box.proposition.Proposition
import scorex.utils.ByteArray

trait EncryTransaction[P <: Proposition, BXP <: Proposition, BB <: BaseBoxBody]
  extends EncryBaseTransaction with JsonSerializable{

  // `scorex.core.transaction.account.PublicKeyNoncedBox` is unsuitable for PKI
  // as a NoncedBox[P] because of hardcoded `PublicKey25519Proposition`.

  // Default `scorex.core.transaction.box.Box` is suitable only for payments
  // being designed to store only `Long` as a value.

  import encry.modifiers.mempool.EncryTransaction._

  // Type of the transaction will be telling the abstract `dispatcher` how to treat particular Txn.

  // `BoxUnlocker` holds ID and Key of the box to open (Sequence of `Tx Inputs` + Keys to unlock them).
  val unlockers: Traversable[BoxUnlocker[P]]
  // Sequence of `Tx Outputs`.
  val newBoxes: Traversable[EncryBaseBox[BXP, BB]]

  val fee: Long
  val timestamp: Long

}

object EncryTransaction {
  // TODO: Make this type `supertagged`.
  type TxTypeId = Byte
  type Nonce = Long
  type Amount = Long

  def nonceFromDigest(digest: Array[Byte]): Nonce = Longs.fromByteArray(digest.take(8))
}
