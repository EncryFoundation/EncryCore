package encry.modifiers.history.block

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.mempool.{EncryBaseTransaction, EncryPaymentTransaction}
import encry.settings.Algos
import io.circe.Json
import io.circe.syntax._
import scorex.core.{ModifierId, ModifierTypeId}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.serialization.Serializer

import scala.util.Try

class EncryBlock(override val header: EncryBlockHeader,
                 override val payload: EncryBlockPayload)
  extends EncryBaseBlock[PublicKey25519Proposition, EncryBaseTransaction[_, _, _], EncryBlockPayload] {

  override type M = EncryBlock

  override val toSeq: Seq[EncryPersistentModifier] = Seq(header, payload)

  override def transactions: Seq[EncryBaseTransaction[_, _, _]] = payload.transactions

  override def parentId: ModifierId = header.parentId

  override val modifierTypeId: ModifierTypeId = EncryBlock.modifierTypeId

  override lazy val id: ModifierId = ModifierId @@ Algos.hash(header.id ++ payload.id)

  override def serializer: Serializer[EncryBlock] = EncryPaymentBlockSerializer

  override lazy val json: Json = Map(
    "header" -> header.json,
    "payload" -> payload.json,
//    "adPoofs" -> aDProofs.map(_.json).getOrElse(Map.empty[String, String].asJson)
  ).asJson
}

object EncryBlock {
  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (-127: Byte)
}

object EncryPaymentBlockSerializer extends Serializer[EncryBlock] {

  override def toBytes(obj: EncryBlock): Array[Byte] = ???

  override def parseBytes(bytes: Array[Byte]): Try[EncryBlock] = ???
}
