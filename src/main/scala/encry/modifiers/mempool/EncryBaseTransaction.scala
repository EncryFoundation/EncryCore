package encry.modifiers.mempool

import encry.account.Account
import encry.modifiers.Signable25519
import encry.modifiers.mempool.EncryBaseTransaction.TxTypeId
import encry.modifiers.state.box.{EncryBaseBox, OpenBox}
import encry.settings.Constants
import scorex.core.ModifierId
import scorex.core.serialization.JsonSerializable
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.Proposition
import scorex.crypto.authds.ADKey
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Digest32

import scala.util.Try

trait EncryBaseTransaction extends Transaction[Proposition]
  with Signable25519 with ModifierWithSizeLimit with JsonSerializable {

  val txHash: Digest32

  val dataToSign: Array[Byte] = txHash

  val semanticValidity: Try[Unit]

  val typeId: TxTypeId

  // override lazy val id: ModifierId = ModifierId @@ (Array[Byte](typeId) ++ txHash)
  override lazy val id: ModifierId = ModifierId @@ txHash.updated(0, typeId)

  val fee: Long

  val timestamp: Long

  val feeBox: Option[OpenBox]

  val unlockers: IndexedSeq[ADKey]

  val newBoxes: Traversable[EncryBaseBox]

  lazy val account: Account = Account(accountPubKey.pubKeyBytes)

  val minimalFee: Float = Constants.feeMinAmount + Constants.txByteCost * length

  override def toString: String = s"<TX: type=$typeId id=${Base58.encode(id)}>"

  // Shadowed.
  override val messageToSign: Array[Byte] = Array.fill(32)(1.toByte)
}

object EncryBaseTransaction {

  type TxTypeId = Byte
  type Nonce = Long
}
