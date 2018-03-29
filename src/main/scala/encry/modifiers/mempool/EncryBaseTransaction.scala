package encry.modifiers.mempool

import encry.account.Account
import encry.modifiers.Signable25519
import encry.modifiers.mempool.directive.Directive
import encry.modifiers.state.box.{EncryBaseBox, OpenBox}
import encry.settings.{Algos, Constants}
import io.circe.Encoder
import scorex.core.ModifierId
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.Proposition
import scorex.crypto.hash.Digest32

import scala.util.Try

trait EncryBaseTransaction extends Transaction[Proposition]
  with Signable25519 with ModifierWithSizeLimit {

  val txHash: Digest32

  val dataToSign: Array[Byte] = txHash

  val semanticValidity: Try[Unit]

  override lazy val id: ModifierId = ModifierId @@ txHash

  val fee: Long

  val timestamp: Long

  val isCoinbase: Boolean

  val feeBox: Option[OpenBox]

  val unlockers: IndexedSeq[Unlocker]

  val directives: IndexedSeq[Directive]

  lazy val newBoxes: Traversable[EncryBaseBox] =
     directives.flatMap(_.boxes(txHash)) ++ feeBox.map(fb => Seq(fb)).getOrElse(Seq.empty)

  lazy val account: Account = Account(accountPubKey.pubKeyBytes)

  lazy val minimalFee: Float = Constants.feeMinAmount + Constants.txByteCost * length

  override def toString: String = s"<TX: id=${Algos.encode(id)} isCoinbase=$isCoinbase>"

  // Shadowed.
  override val messageToSign: Array[Byte] = Array.fill(32)(1.toByte)
}

object EncryBaseTransaction {

  type TxTypeId = Byte
  type Nonce = Long

  implicit val jsonEncoder: Encoder[EncryBaseTransaction] = {
    case tx: EncryTransaction => EncryTransaction.jsonEncoder(tx)
  }
}
