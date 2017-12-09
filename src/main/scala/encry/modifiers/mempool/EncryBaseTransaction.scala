package encry.modifiers.mempool

import com.google.common.primitives.{Bytes, Longs}
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.{Box, BoxUnlocker}
import scorex.core.transaction.box.proposition.Proposition
import scorex.utils.ByteArray

abstract class ErgoBaseTransaction[P <: Proposition, BX <: Box[P]] extends Transaction[P] {

  // TODO: Implement custom `NoncedBox`
  // `scorex.core.transaction.account.PublicKeyNoncedBox` is unsuitable
  // as a NoncedBox[P] because of hardcoded `PublicKey25519Proposition`.

  // TODO: Implement custom `Box` to store complex data.
  // Default `scorex.core.transaction.box.Box` is unsuitable being designed to store only `Long` as a value.

  import encry.modifiers.mempool.ErgoBaseTransaction._

  // Type of the transaction will be telling the abstract `dispatcher` how to treat txn.
  val typeId: TxTypeId

  // `BoxUnlocker` holds ID and Key of the box to open.
  val unlockers: Traversable[BoxUnlocker[P]]
  val newBoxes: Traversable[BX]

  val fee: Long
  val timestamp: Long

  override lazy val messageToSign: Array[Byte] =
    Bytes.concat(
      Array[Byte](typeId),
      if (newBoxes.nonEmpty) scorex.core.utils.concatBytes(newBoxes.map(_.bytes)) else Array[Byte](),
      scorex.core.utils.concatFixLengthBytes(unlockers.map(_.closedBoxId)),
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee))

}

object ErgoBaseTransaction {
  // TODO: Make this type `supertagged`.
  type TxTypeId = Byte
}
