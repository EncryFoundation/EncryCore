package encry.modifiers.history.block.payload

import com.google.common.primitives.{Bytes, Ints}
import encry.modifiers.mempool._
import encry.modifiers.serialization.Serializer
import encry.settings.Algos
import encry.{ModifierId, ModifierTypeId}
import io.circe.Encoder
import io.circe.syntax._
import scorex.crypto.authds.LeafData
import scorex.crypto.hash.Digest32

import scala.util.Try

case class EncryBlockPayload(override val headerId: ModifierId, txs: Seq[EncryBaseTransaction])
  extends EncryBaseBlockPayload {

  assert(txs.nonEmpty, "Block should contain at least 1 coinbase-like transaction")

  override val modifierTypeId: ModifierTypeId = EncryBlockPayload.modifierTypeId

  override type M = EncryBlockPayload

  override val transactions: Seq[EncryBaseTransaction] = txs

  override lazy val digest: Digest32 = EncryBlockPayload.rootHash(txs.map(_.id))

  override def serializer: Serializer[EncryBlockPayload] = EncryBlockPayloadSerializer

  override def toString: String = s"Payload(headerId=${Algos.encode(headerId)}, txsQty=${transactions.size})"
}

object EncryBlockPayload {

  implicit val jsonEncoder: Encoder[EncryBlockPayload] = (bp: EncryBlockPayload) => Map(
    "headerId" -> Algos.encode(bp.headerId).asJson,
    "transactions" -> bp.txs.map(_.asJson).asJson
  ).asJson

  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (102: Byte)

  def rootHash(ids: Seq[ModifierId]): Digest32 = Algos.merkleTreeRoot(LeafData !@@ ids)
}

object EncryBlockPayloadSerializer extends Serializer[EncryBlockPayload] {

  override def toBytes(obj: EncryBlockPayload): Array[Byte] =
    Bytes.concat(
      obj.headerId,
      Ints.toByteArray(obj.transactions.size),
      obj.transactions.map(tx => Ints.toByteArray(tx.bytes.length) ++ tx.bytes).reduceLeft(_ ++ _)
    )

  override def parseBytes(bytes: Array[Byte]): Try[EncryBlockPayload] = Try {
    val headerId: Array[Byte] = bytes.slice(0, 32)
    val txQty: Int = Ints.fromByteArray(bytes.slice(32, 36))
    val leftBytes: Array[Byte] = bytes.drop(36)
    val txs: Seq[EncryBaseTransaction] = (0 until txQty).foldLeft(Seq[EncryBaseTransaction](), 0) { case ((acc, shift), _) =>
      val len: Int = Ints.fromByteArray(leftBytes.slice(shift, shift + 4))
      EncryTransactionSerializer
        .parseBytes(leftBytes.slice(shift + 4, shift + 4 + len)).map(d => (acc :+ d, shift + 4 + len))
        .getOrElse(throw new Exception("Serialization failed."))
    }._1
    EncryBlockPayload(ModifierId @@ headerId, txs)
  }
}
