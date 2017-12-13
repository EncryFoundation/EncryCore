package encry.modifiers.history.block.payload

import encry.modifiers.mempool.{EncryBaseTransaction, EncryPaymentTransaction}
import encry.settings.Algos
import io.circe.Json
import io.circe.syntax._
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.{ModifierId, ModifierTypeId}
import scorex.crypto.authds.LeafData
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Digest32

import scala.util.Try

class EncryBlockPayload(override val headerId: ModifierId, override val txs: Seq[EncryBaseTransaction[_, _, _]])
  extends EncryBaseBlockPayload[PublicKey25519Proposition, EncryPaymentTransaction] {

  assert(txs.nonEmpty, "Block should contain at least 1 coinbase-like transaction")

  override val modifierTypeId: ModifierTypeId = EncryBlockPayload.modifierTypeId

  override type M = EncryBlockPayload

  override def digest: Digest32 = EncryBlockPayload.rootHash(txs.map(_.id))

  override lazy val transactions: Seq[EncryBaseTransaction[_, _, _]] = txs

  override lazy val json: Json = Map(
    "headerId" -> Base58.encode(headerId).asJson,
    "transactions" -> txs.map(_.json).asJson
  ).asJson

  override def serializer: Serializer[EncryBlockPayload] = EncryPaymentBlockPayloadSerializer

}

object EncryBlockPayload {

  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (102: Byte)

  def rootHash(ids: Seq[ModifierId]): Digest32 = Algos.merkleTreeRoot(LeafData @@ ids)
}

object EncryPaymentBlockPayloadSerializer extends Serializer[EncryBlockPayload] {

  override def toBytes(obj: EncryBlockPayload): Array[Byte] = ???

  override def parseBytes(bytes: Array[Byte]): Try[EncryBlockPayload] = ???
}
