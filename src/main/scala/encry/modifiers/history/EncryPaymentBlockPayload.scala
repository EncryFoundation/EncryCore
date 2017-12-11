package encry.modifiers.history

import encry.modifiers.mempool.EncryPaymentTransaction
import encry.settings.Algos
import io.circe.Json
import io.circe.syntax._
import scorex.core.{ModifierId, ModifierTypeId}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.serialization.Serializer
import scorex.crypto.authds.LeafData
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Digest32

import scala.util.Try

class EncryPaymentBlockPayload(override val headerId: ModifierId, override val txs: Seq[EncryPaymentTransaction])
  extends EncryBaseBlockPayload[PublicKey25519Proposition, EncryPaymentTransaction] {

  assert(txs.nonEmpty, "Block should contain at least 1 coinbase-like transaction")

  override val modifierTypeId: ModifierTypeId = EncryPaymentBlockPayload.modifierTypeId

  override type M = EncryPaymentBlockPayload

  override def digest: Digest32 = EncryPaymentBlockPayload.rootHash(txs.map(_.id))

  override lazy val transactions: Seq[EncryPaymentTransaction] = txs

  override lazy val json: Json = Map(
    "headerId" -> Base58.encode(headerId).asJson,
    "transactions" -> txs.map(_.json).asJson
  ).asJson

  override def serializer: Serializer[EncryPaymentBlockPayload] = EncryPaymentBlockPayloadSerializer

}

object EncryPaymentBlockPayload {

  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (102: Byte)

  def rootHash(ids: Seq[ModifierId]): Digest32 = Algos.merkleTreeRoot(LeafData @@ ids)
}

object EncryPaymentBlockPayloadSerializer extends Serializer[EncryPaymentBlockPayload] {

  override def toBytes(obj: EncryPaymentBlockPayload): Array[Byte] = ???

  override def parseBytes(bytes: Array[Byte]): Try[EncryPaymentBlockPayload] = ???
}
