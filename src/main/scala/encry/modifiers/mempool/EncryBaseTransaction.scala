package encry.modifiers.mempool

import com.google.common.primitives.Ints
import encry.ModifierId
import encry.modifiers.history.block.Block.Timestamp
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.mempool.directive.Directive
import encry.modifiers.state.box.Box.Amount
import encry.modifiers.state.box.{AssetBox, DataBox, EncryBaseBox, EncryProposition}
import encry.settings.{Algos, Constants}
import io.circe.Encoder
import org.encryfoundation.prismlang.compiler.CompiledContract
import org.encryfoundation.prismlang.core.PConvertible
import scorex.crypto.encode.Base16
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
    inputs.map(_.contract.fold(cc => cc, rc => rc.contract)).map(CompiledContract.costOf).sum +
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

case class TransactionDBVersion(id: String, blockId: String, isCoinbase: String, timestamp: Timestamp)

case object TransactionDBVersion {
  def apply(block: EncryBlock): Seq[TransactionDBVersion] = {
    assert(block.payload.transactions.nonEmpty)
    val transactions = block.payload.transactions.map { tx =>
      val id: String = Base16.encode(tx.id)
      val blockId: String = Base16.encode(block.header.id)
      TransactionDBVersion(id, blockId, "FALSE", block.header.timestamp)
    }.toIndexedSeq
    transactions.init :+ transactions.last.copy(isCoinbase = "TRUE")
  }
}


case class InputDBVersion(id: String, txId: String, proofs: String)

case object InputDBVersion {
  def apply(tx: EncryBaseTransaction): Seq[InputDBVersion] = {
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

case class OutputDBVersion(id: String, txId: String, monetaryValue: Long, coinId: String, contractHash: String, data: String)

object OutputDBVersion {
  def apply(tx: EncryBaseTransaction): Seq[OutputDBVersion] = {
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
      OutputDBVersion(id, txId, monetaryValue, coinId, contractHash, data)
    }.toIndexedSeq
  }
}

