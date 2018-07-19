package encry.modifiers.mempool

import com.google.common.primitives.Ints
import encry.ModifierId
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

  def outputsString: String = {
    val txId: String = Base16.encode(id)
    val outputs: IndexedSeq[String] = newBoxes.map { bx =>
      val id: String = Base16.encode(bx.id)
      val (monetaryValue: Long, coinId: String, dataOpt: Option[Array[Byte]]) = bx match {
        case ab: AssetBox => (ab.amount, Base16.encode(ab.tokenIdOpt.getOrElse(Constants.IntrinsicTokenId)), None)
        case db: DataBox => (0L, Base16.encode(Constants.IntrinsicTokenId), db.data)
        case _ => (0L, Base16.encode(Constants.IntrinsicTokenId), None)
      }
      val data: String = dataOpt.map(Base16.encode).getOrElse("")
      val contractHash: String = Base16.encode(bx.proposition.contractHash)
      s"('$id', '$txId', '$monetaryValue', '$coinId', '$contractHash', '$data')"
    }.toIndexedSeq
    outputs.mkString(", ")
  }

  def inputsString: String = {
    val txId: String = Base16.encode(id)
    inputs.map { in =>
      val id: String = Base16.encode(in.boxId)
      val proofs: String = Base16.encode(in.proofs.map { proof =>
        val proofBytes: Array[Byte] = proof.bytes
        Ints.toByteArray(proofBytes.length) ++ proofBytes
      }.foldLeft(Array.empty[Byte])(_ ++ _))
      s"('$id', '$txId', '$proofs')"
    }.mkString(", ")
  }
}

object EncryBaseTransaction {

  type TxTypeId = Byte
  type Nonce = Long

  case class TransactionValidationException(s: String) extends Exception(s)

  implicit val jsonEncoder: Encoder[EncryBaseTransaction] = {
    case tx: EncryTransaction => EncryTransaction.jsonEncoder(tx)
  }
}
