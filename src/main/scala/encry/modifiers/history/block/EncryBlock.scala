package encry.modifiers.history.block

import com.google.common.primitives.{Bytes, Ints}
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.header.{EncryBlockHeader, EncryBlockHeaderSerializer}
import encry.modifiers.history.block.payload.{EncryBlockPayload, EncryBlockPayloadSerializer}
import encry.modifiers.history.{ADProofSerializer, ADProofs}
import encry.modifiers.mempool.EncryBaseTransaction
import io.circe.Encoder
import io.circe.syntax._
import scorex.core.serialization.Serializer
import scorex.core.{ModifierId, ModifierTypeId}

import scala.util.{Failure, Success, Try}

case class EncryBlock(override val header: EncryBlockHeader,
                      override val payload: EncryBlockPayload,
                      adProofsOpt: Option[ADProofs]) extends EncryBaseBlock {

  override type M = EncryBlock

  override val toSeq: Seq[EncryPersistentModifier] = Seq(header, payload) ++ adProofsOpt.toSeq

  override def transactions: Seq[EncryBaseTransaction] = payload.transactions

  override def semanticValidity: Try[Unit] = {
    if (header.transactionsRoot != payload.digest) Failure(new Exception("Invalid payload root hash"))
    else if (!header.validSignature) Failure(new Exception("Invalid signature"))
    else Success()
  }

  override def parentId: ModifierId = header.parentId

  override val modifierTypeId: ModifierTypeId = EncryBlock.modifierTypeId

  override lazy val id: ModifierId = header.id

  override def serializer: Serializer[EncryBlock] = EncryBlockSerializer
}

object EncryBlock {

  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (100: Byte)

  implicit val jsonEncoder: Encoder[EncryBlock] = (b: EncryBlock) => Map(
    "header" -> b.header.asJson,
    "payload" -> b.payload.asJson,
    "adProofs" -> b.adProofsOpt.map(_.asJson).getOrElse(Map.empty[String, String].asJson)
  ).asJson
}

object EncryBlockSerializer extends Serializer[EncryBlock] {

  override def toBytes(obj: EncryBlock): Array[Byte] = {
    val headerBytes = obj.header.serializer.toBytes(obj.header)
    val payloadBytes = obj.payload.serializer.toBytes(obj.payload)
    val aDProofsBytes = obj.adProofsOpt.map(_.serializer.toBytes(obj.adProofsOpt.get)).getOrElse(Array.emptyByteArray)
    Bytes.concat(
      Ints.toByteArray(headerBytes.length),
      headerBytes,
      Ints.toByteArray(payloadBytes.length),
      payloadBytes,
      Ints.toByteArray(aDProofsBytes.length),
      aDProofsBytes
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[EncryBlock] = Try{
    var pointer = 4
    val headerSize = Ints.fromByteArray(bytes.slice(0, pointer))
    val header = EncryBlockHeaderSerializer.parseBytes(bytes.slice(pointer, pointer + headerSize))
    pointer += headerSize
    val payloadSize = Ints.fromByteArray(bytes.slice(pointer, pointer + 4))
    val payload = EncryBlockPayloadSerializer.parseBytes(bytes.slice(pointer + 4, pointer + 4 + payloadSize))
    pointer += payloadSize + 4
    val aDProofsSize = Ints.fromByteArray(bytes.slice(pointer, pointer + 4))
    val aDProofs = ADProofSerializer.parseBytes(bytes.slice(pointer + 4, pointer + 4 + aDProofsSize))
    new EncryBlock(header.get, payload.get, Option(aDProofs.get))
  }
}
