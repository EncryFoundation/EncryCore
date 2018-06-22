package encry.modifiers.mempool

import encry.modifiers.mempool.directive.Directive
import encry.modifiers.state.box.EncryBaseBox
import encry.modifiers.state.box.proposition.EncryProposition
import encry.settings.{Algos, Constants}
import io.circe.Encoder
import org.encryfoundation.prismlang.core.PConvertible
import encry.ModifierId
import encry.modifiers.state.box.Box.Amount
import scorex.crypto.hash.Digest32

import scala.util.Try

trait EncryBaseTransaction extends Transaction[EncryProposition] with ModifierWithSizeLimit with PConvertible {

  val messageToSign: Array[Byte]
  val semanticValidity: Try[Unit]
  override lazy val id: ModifierId = ModifierId !@@ Algos.hash(messageToSign)
  val fee: Long
  val timestamp: Long
  val inputs: IndexedSeq[Input]
  val directives: IndexedSeq[Directive]
  val defaultProofOpt: Option[Proof]
  lazy val newBoxes: Traversable[EncryBaseBox] =
    directives.zipWithIndex.flatMap { case (d, idx) => d.boxes(Digest32 !@@ id, idx) }
  //Todo: resolve
  lazy val minimalFee: Amount = Constants.FeeMinAmount +
    directives.map(_.cost).sum + (Constants.PersistentByteCost * length)

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
