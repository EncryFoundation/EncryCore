package encry.modifiers.history

import PayloadProto.PayloadProtoMessage
import com.google.common.primitives.{Bytes, Ints}
import com.google.protobuf.ByteString
import encry.modifiers.mempool._
import encry.modifiers.state.box.EncryProposition
import encry.modifiers.{EncryPersistentModifier, ModifierWithDigest, TransactionsCarryingPersistentNodeViewModifier}
import encry.utils.CoreTaggedTypes.{ModifierId, ModifierTypeId}
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import org.apache.commons.lang.ArrayUtils
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
    "headerId"     -> Algos.encode(bp.headerId).asJson,
    "transactions" -> bp.txs.map(_.asJson).asJson
  ).asJson

  implicit val jsonDecoder: Decoder[Payload] = (c: HCursor) => {
    for {
      headerId     <- c.downField("headerId").as[String]
      transactions <- c.downField("transactions").as[Seq[Transaction]]
    } yield Payload(
      ModifierId @@ Algos.decode(headerId).getOrElse(Array.emptyByteArray),
      transactions
    )
  }

  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (102: Byte)

  def rootHash(ids: Seq[ModifierId]): Digest32 = Algos.merkleTreeRoot(LeafData !@@ ids)

  def toProto(payload: Payload): PayloadProtoMessage = PayloadProtoMessage()
    .withHeaderId(ByteString.copyFrom(payload.headerId))
    .withTxs(payload.transactions.map(x => Transaction.toProto(x)))

  def fromProto(message: Array[Byte]): Try[Payload] = Try {
    val payloadProtoMessage: PayloadProtoMessage = PayloadProtoMessage.parseFrom(message)
    Payload(
      ModifierId @@ payloadProtoMessage.headerId.toByteArray,
      Seq.empty
//      payloadProtoMessage.txs.map(x => Transaction.fromProto(x).get)
    )
  }
}

object PayloadSerializer extends Serializer[Payload] {

  override def toBytes(obj: Payload): Array[Byte] =
    Bytes.concat(
      obj.headerId,
      Ints.toByteArray(obj.transactions.size),
      obj.transactions.map(tx => ArrayUtils.addAll(Ints.toByteArray(tx.bytes.length),tx.bytes))
        .foldLeft(Array.emptyByteArray){case (acc, txBytes) => ArrayUtils.addAll(acc, txBytes)}
    )

  override def parseBytes(bytes: Array[Byte]): Try[Payload] = Try {
    val headerId: Array[Byte] = bytes.slice(0, 32)
    val txQty: Int = Ints.fromByteArray(bytes.slice(32, 36))
    val leftBytes: Array[Byte] = bytes.drop(36)
    val txs: Seq[Transaction] = (0 until txQty).foldLeft(List[Transaction](), 0) { case ((acc, shift), _) =>
      val len: Int = Ints.fromByteArray(leftBytes.slice(shift, shift + 4))
      TransactionSerializer
        .parseBytes(ArrayUtils.subarray(leftBytes, shift + 4, shift + 4 + len)).map(d => (d :: acc, shift + 4 + len))
        .getOrElse(throw new Exception("Serialization failed."))
    }._1.reverse
    Payload(ModifierId @@ headerId, txs)
  }
}
