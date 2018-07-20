package encry.modifiers.mempool

import encry.modifiers.NodeViewModifier
import encry.{ModifierId, ModifierTypeId}
import encry.modifiers.mempool.directive.Directive
import encry.modifiers.state.box.Box.Amount
import encry.modifiers.state.box.EncryBaseBox
import encry.settings.{Algos, Constants}
import io.circe.Encoder
import org.encryfoundation.prismlang.compiler.CompiledContract
import org.encryfoundation.prismlang.core.PConvertible
import scorex.crypto.hash.Digest32
import scala.util.Try

trait BaseTransaction extends NodeViewModifier with PConvertible {

  val modifierTypeId: ModifierTypeId = BaseTransaction.ModifierTypeId
  val messageToSign: Array[Byte]
  val id: ModifierId
  val fee: Long
  val timestamp: Long
  val inputs: IndexedSeq[Input]
  val directives: IndexedSeq[Directive]
  val defaultProofOpt: Option[Proof]
  val semanticValidity: Try[Unit]
  val length: Int = this.bytes.length
  val maxSize: Int = Constants.TransactionMaxSize
  lazy val newBoxes: Traversable[EncryBaseBox] =
    directives.zipWithIndex.flatMap { case (d, idx) => d.boxes(Digest32 !@@ id, idx) }
  lazy val costMultiplier: Amount =
    inputs.map(_.contract.fold(cc => cc, rc => rc.contract)).map(CompiledContract.costOf).sum +
    (Constants.PersistentByteCost * length) +
    (Constants.StateByteCost * newBoxes.map(_.bytes).foldLeft(Array.empty[Byte])(_ ++ _).length)

  override def toString: String = s"<EncryTransaction id=${Algos.encode(id)} fee=$fee inputs=${inputs.map(u => Algos.encode(u.boxId))}>"
}

object BaseTransaction {

  type TxTypeId = Byte
  type Nonce = Long

  case class TransactionValidationException(s: String) extends Exception(s)

  implicit val jsonEncoder: Encoder[BaseTransaction] = {
    case tx: EncryTransaction => EncryTransaction.jsonEncoder(tx)
  }

  val ModifierTypeId: ModifierTypeId = encry.ModifierTypeId @@ 2.toByte
}
