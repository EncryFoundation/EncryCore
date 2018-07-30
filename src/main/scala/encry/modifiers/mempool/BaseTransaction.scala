package encry.modifiers.mempool

import com.google.common.primitives.Ints
import encry.modifiers.history.block.Block.Timestamp
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.NodeViewModifier
import encry.{ModifierId, ModifierTypeId}
import encry.modifiers.mempool.directive.Directive
import encry.modifiers.state.box.Box.Amount
import encry.modifiers.state.box.{AssetBox, DataBox}
import encry.modifiers.state.box.EncryBaseBox
import encry.settings.{Algos, Constants}
import io.circe.Encoder
import org.encryfoundation.prismlang.compiler.CompiledContract
import org.encryfoundation.prismlang.core.PConvertible
import scorex.crypto.encode.Base16
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

case class TransactionDBVersion(id: String, blockId: String, isCoinbase: Boolean, timestamp: Timestamp)

case object TransactionDBVersion {
  def apply(block: EncryBlock): Seq[TransactionDBVersion] = {
    if (block.payload.transactions.nonEmpty) {
      val transactions: Seq[TransactionDBVersion] = block.payload.transactions.map { tx =>
        val id: String = Base16.encode(tx.id)
        val blockId: String = Base16.encode(block.header.id)
        TransactionDBVersion(id, blockId, isCoinbase = false, block.header.timestamp)
      }.toIndexedSeq
      transactions.init :+ transactions.last.copy(isCoinbase = true)
    } else Seq.empty
  }
}


case class InputDBVersion(id: String, txId: String, proofs: String)

case object InputDBVersion {
  def apply(tx: BaseTransaction): Seq[InputDBVersion] = {
    val txId: String = Base16.encode(tx.id)
    tx.inputs.map { in =>
      val id: String = Base16.encode(in.boxId)
      val proofs: String = Base16.encode(in.proofs.map { proof =>
        val proofBytes: Array[Byte] = proof.bytes
        Ints.toByteArray(proofBytes.length) ++ proofBytes
      }.foldLeft(Array.empty[Byte])(_ ++ _))
      InputDBVersion(id, txId, proofs)
    }
  }
}

case class OutputDBVersion(id: String, txId: String, monetaryValue: Long, coinId: String, contractHash: String, unspent: Boolean, data: String)

object OutputDBVersion {
  def apply(tx: BaseTransaction): Seq[OutputDBVersion] = {
    val txId: String = Base16.encode(tx.id)
    tx.newBoxes.map { bx =>
      val id: String = Base16.encode(bx.id)
      val (monetaryValue: Long, coinId: String, dataOpt: Option[Array[Byte]]) = bx match {
        case ab: AssetBox => (ab.amount, Base16.encode(ab.tokenIdOpt.getOrElse(Constants.IntrinsicTokenId)), None)
        case db: DataBox => (0L, Base16.encode(Constants.IntrinsicTokenId), db.data)
        case _ => (0L, Base16.encode(Constants.IntrinsicTokenId), None)
      }
      val data: String = dataOpt.map(Base16.encode).getOrElse("")
      val contractHash: String = Base16.encode(bx.proposition.contractHash)
      OutputDBVersion(id, txId, monetaryValue, coinId, contractHash, true, data)
    }.toIndexedSeq
  }
}

