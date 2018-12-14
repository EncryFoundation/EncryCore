package encry.modifiers.history

import com.google.common.primitives.{Bytes, Ints}
import encry.modifiers.mempool._
import encry.modifiers.state.box.EncryProposition
import encry.modifiers.{EncryPersistentModifier, ModifierWithDigest, TransactionsCarryingPersistentNodeViewModifier}
import encry.utils.CoreTaggedTypes.{ModifierId, ModifierTypeId}
import io.circe.Encoder
import io.circe.syntax._
import org.encryfoundation.common.Algos
import org.encryfoundation.common.serialization.Serializer
import org.encryfoundation.common.utils.TaggedTypes.LeafData
import scorex.crypto.hash.Digest32
import scala.util.Try

case class Payload(override val headerId: ModifierId, txs: Seq[Transaction])
  extends TransactionsCarryingPersistentNodeViewModifier[EncryProposition, Transaction]
    with EncryPersistentModifier
    with ModifierWithDigest {

  assert(txs.nonEmpty, "Block should contain at least 1 coinbase-like transaction")

  override val modifierTypeId: ModifierTypeId = Payload.modifierTypeId

  override type M = Payload

  override val transactions: Seq[Transaction] = txs

  override lazy val digest: Digest32 = Payload.rootHash(txs.map(_.id))

  override def serializer: Serializer[Payload] = PayloadSerializer

  override def toString: String = s"Payload(headerId=${Algos.encode(headerId)}," +
    s" txsQty=${transactions.size}, id = ${Algos.encode(id)})"
}

object Payload {

  implicit val jsonEncoder: Encoder[Payload] = (bp: Payload) => Map(
    "headerId" -> Algos.encode(bp.headerId).asJson,
    "transactions" -> bp.txs.map(_.asJson).asJson
  ).asJson

  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (102: Byte)

  def rootHash(ids: Seq[ModifierId]): Digest32 = Algos.merkleTreeRoot(LeafData !@@ ids)
}

object PayloadSerializer extends Serializer[Payload] {

  override def toBytes(obj: Payload): Array[Byte] =
    Bytes.concat(
      obj.headerId,
      Ints.toByteArray(obj.transactions.size),
      obj.transactions.map(tx => Ints.toByteArray(tx.bytes.length) ++ tx.bytes).reduceLeft(_ ++ _)
    )

  override def parseBytes(bytes: Array[Byte]): Try[Payload] = Try {
    val headerId: Array[Byte] = bytes.slice(0, 32)
    val txQty: Int = Ints.fromByteArray(bytes.slice(32, 36))
    val leftBytes: Array[Byte] = bytes.drop(36)
    val txs: Seq[Transaction] = (0 until txQty).foldLeft(Seq[Transaction](), 0) { case ((acc, shift), _) =>
      val len: Int = Ints.fromByteArray(leftBytes.slice(shift, shift + 4))
      TransactionSerializer
        .parseBytes(leftBytes.slice(shift + 4, shift + 4 + len)).map(d => (acc :+ d, shift + 4 + len))
        .getOrElse(throw new Exception("Serialization failed."))
    }._1
    Payload(ModifierId @@ headerId, txs)
  }
}
