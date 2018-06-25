package encry.modifiers.mempool

import encry.modifiers.mempool.directive.Directive
import encry.modifiers.state.box.{EncryBaseBox, EncryProposition}
import encry.settings.{Algos, Constants}
import io.circe.Encoder
import org.encryfoundation.prismlang.core.PConvertible
import encry.ModifierId
import encry.modifiers.state.box.Box.Amount
import scorex.crypto.hash.Digest32

import scala.util.Try

trait EncryBaseTransaction extends Transaction[EncryProposition] with ModifierWithSizeLimit with PConvertible {

  override lazy val id: ModifierId = ModifierId !@@ Algos.hash(messageToSign)

  val fee: Long
  val timestamp: Long
  val inputs: IndexedSeq[Input]
  val directives: IndexedSeq[Directive]
  val defaultProofOpt: Option[Proof]

  val messageToSign: Array[Byte]
  val semanticValidity: Try[Unit]

  lazy val newBoxes: Traversable[EncryBaseBox] =
    directives.zipWithIndex.flatMap { case (d, idx) => d.boxes(Digest32 !@@ id, idx) }

  lazy val costMultiplier: Amount =
    // spending scripts cost +
    (Constants.PersistentByteCost * length) +
    (Constants.StateByteCost * newBoxes.map(_.bytes).foldLeft(Array.empty[Byte])(_ ++ _).length)

  override def toString: String = s"<EncryTransaction id=${Algos.encode(id)} fee=$fee inputs=${inputs.map(u => Algos.encode(u.boxId))}>"
}

object EncryBaseTransaction {

  type TxTypeId = Byte
  type Nonce = Long

  case class TransactionValidationException(s: String) extends Exception(s)

  implicit val jsonEncoder: Encoder[EncryBaseTransaction] = {
    case tx: EncryTransaction => EncryTransaction.jsonEncoder(tx)
  }
}
