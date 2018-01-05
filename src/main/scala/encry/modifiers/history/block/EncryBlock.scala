package encry.modifiers.history.block

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.ADProofs
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.mempool.EncryBaseTransaction
import encry.settings.Algos
import io.circe.Json
import io.circe.syntax._
import scorex.core.{ModifierId, ModifierTypeId}
import scorex.core.serialization.Serializer

import scala.util.{Failure, Success, Try}

class EncryBlock(override val header: EncryBlockHeader,
                 override val payload: EncryBlockPayload,
                 val adProofsOpt: Option[ADProofs]) extends EncryBaseBlock {

  override type M = EncryBlock

  override val toSeq: Seq[EncryPersistentModifier] = Seq(header, payload) ++ adProofsOpt.toSeq

  override def transactions: Seq[EncryBaseTransaction] = payload.transactions

  override def semanticValidity: Try[Unit] = {
    // TODO: Make transactions validity checks here?
    if (header.txsRoot != payload.digest) {
      log.info(s"<BLOCK ${header.id}> Invalid tx Merkle Root hash provided.")
      Failure(new Error("Invalid tx Merkle Root hash provided!"))
    }
    if (!header.validTimestamp) {
      log.info(s"<BLOCK ${header.id}> Invalid timestamp provided.")
      Failure(new Error("Invalid timestamp provided!"))
    }
    Success()
  }

  override def parentId: ModifierId = header.parentId

  override val modifierTypeId: ModifierTypeId = EncryBlock.modifierTypeId

  override lazy val id: ModifierId = ModifierId @@ Algos.hash(header.id ++ payload.id)

  override def serializer: Serializer[EncryBlock] = EncryPaymentBlockSerializer

  override lazy val json: Json = Map(
    "header" -> header.json,
    "payload" -> payload.json,
    "adPoofs" -> adProofsOpt.map(_.json).getOrElse(Map.empty[String, String].asJson)
  ).asJson
}

object EncryBlock {
  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (-127: Byte)
}

object EncryPaymentBlockSerializer extends Serializer[EncryBlock] {

  override def toBytes(obj: EncryBlock): Array[Byte] = ???

  override def parseBytes(bytes: Array[Byte]): Try[EncryBlock] = ???
}
