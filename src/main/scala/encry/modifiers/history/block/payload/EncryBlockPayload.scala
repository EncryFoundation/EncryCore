package encry.modifiers.history.block.payload

import com.google.common.primitives.{Bytes, Ints}
import encry.modifiers.mempool._
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
      obj.transactions.foldLeft(Seq[Array[Byte]]()){ case (arr, tx) =>
        arr :+ (Ints.toByteArray(tx.bytes.length) ++ Array[Byte](tx.typeId) ++ tx.bytes)
      }.flatten.toArray
    )

  override def parseBytes(bytes: Array[Byte]): Try[EncryBlockPayload] = Try {
    val headerId = bytes.slice(0, 32)
    val txQty = Ints.fromByteArray(bytes.slice(32, 36))
    var slicePointer = 36
    val txs = (0 until txQty).foldLeft(Seq[EncryBaseTransaction]()) { case (seq, _) =>
      val txSize = Ints.fromByteArray(bytes.slice(slicePointer, slicePointer + 4))
      val txTypeId = bytes.slice(slicePointer + 4, slicePointer + 5).head
      val tx: Option[EncryBaseTransaction] = txTypeId match {
        case PaymentTransaction.typeId =>
          Some(PaymentTransactionSerializer.parseBytes(bytes.slice(slicePointer + 5, slicePointer + 5 + txSize)).get)
        case CoinbaseTransaction.typeId =>
          Some(CoinbaseTransactionSerializer.parseBytes(bytes.slice(slicePointer + 5, slicePointer + 5 + txSize)).get)
        case _ =>
          Failure(new Error("Got unhandled modifier."))
          None
      }
      slicePointer += 5 + txSize
      if (tx.isDefined) seq :+ tx.get
      else seq
    }
    new EncryBlockPayload(ModifierId @@ headerId, txs)
  }
}
