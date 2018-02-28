package encry.modifiers.history.block.payload

import com.google.common.primitives.{Bytes, Ints}
import encry.modifiers.mempool._
import encry.modifiers.mempool.directive.DirectiveSerializer
import encry.settings.Algos
import io.circe.Json
import io.circe.syntax._
import scorex.core.serialization.Serializer
import scorex.core.{ModifierId, ModifierTypeId}
import scorex.crypto.authds.LeafData
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Digest32

import scala.util.{Failure, Try}

class EncryBlockPayload(override val headerId: ModifierId, txs: Seq[EncryBaseTransaction])
  extends EncryBaseBlockPayload {

  assert(txs.nonEmpty, "Block should contain at least 1 coinbase-like transaction")

  override val modifierTypeId: ModifierTypeId = EncryBlockPayload.modifierTypeId

  override type M = EncryBlockPayload

  override val transactions: Seq[EncryBaseTransaction] = txs

  override lazy val digest: Digest32 = EncryBlockPayload.rootHash(txs.map(_.id))

  override lazy val json: Json = Map(
    "headerId" -> Base58.encode(headerId).asJson,
    "transactions" -> txs.map(_.json).asJson
  ).asJson

  override def serializer: Serializer[EncryBlockPayload] = EncryBlockPayloadSerializer

}

object EncryBlockPayload {

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
      EncryTransactionSerializer.parseBytes(leftBytes.slice(shift + 4, shift + 4 + len)).map(d => (acc :+ d, shift + len))
        .getOrElse(throw new Exception("Serialization failed."))
    }._1
    new EncryBlockPayload(ModifierId @@ headerId, txs)
  }
}
