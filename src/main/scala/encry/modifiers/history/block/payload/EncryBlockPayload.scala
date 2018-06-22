package encry.modifiers.history.block.payload

import com.google.common.primitives.{Bytes, Ints}
import encry.modifiers.Serializer
import encry.modifiers.mempool._
import encry.settings.Algos
import io.circe.Encoder
import io.circe.syntax._
import encry.{ModifierId, ModifierTypeId}
import scorex.crypto.authds.LeafData
import scorex.crypto.encode.Base58
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

}

object EncryBlockPayload {

  implicit val jsonEncoder: Encoder[EncryBlockPayload] = (bp: EncryBlockPayload) => Map(
    "headerId" -> Base58.encode(bp.headerId).asJson,
    "transactions" -> bp.txs.map(_.asJson).asJson
  ).asJson

  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (102: Byte)

  def rootHash(ids: Seq[ModifierId]): Digest32 = Algos.merkleTreeRoot(LeafData @@ ids)
}

object EncryBlockPayloadSerializer extends Serializer[EncryBlockPayload] {

  override def toBytes(obj: EncryBlockPayload): Array[Byte] =
    Bytes.concat(
      obj.headerId,
      Ints.toByteArray(obj.transactions.size),
      obj.transactions.map(tx => Ints.toByteArray(tx.bytes.length) ++ tx.bytes).reduceLeft(_ ++ _)
    )

  override def parseBytes(bytes: Array[Byte]): Try[EncryBlockPayload] = Try {
    val headerId = bytes.slice(0, 32)
    val txQty = Ints.fromByteArray(bytes.slice(32, 36))
    val leftBytes = bytes.drop(36)
    val txs = (0 until txQty).foldLeft(Seq[EncryBaseTransaction](), 0) { case ((acc, shift), _) =>
      val len = Ints.fromByteArray(leftBytes.slice(shift, shift + 4))
      EncryTransactionSerializer
        .parseBytes(leftBytes.slice(shift + 4, shift + 4 + len)).map(d => (acc :+ d, shift + 4 + len))
        .getOrElse(throw new Exception("Serialization failed."))
    }._1
    new EncryBlockPayload(ModifierId @@ headerId, txs)
  }
}
