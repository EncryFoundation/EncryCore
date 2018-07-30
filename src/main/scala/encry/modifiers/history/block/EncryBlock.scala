package encry.modifiers.history.block

import com.google.common.primitives.{Bytes, Ints}
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.header.{EncryBlockHeader, EncryBlockHeaderSerializer}
import encry.modifiers.history.block.payload.{EncryBlockPayload, EncryBlockPayloadSerializer}
import encry.modifiers.history.{ADProofSerializer, ADProofs}
import encry.modifiers.mempool.directive.TransferDirective
import encry.modifiers.mempool.BaseTransaction
import encry.modifiers.serialization.Serializer
import encry.validation.{ModifierValidator, ValidationResult}
import encry.{ModifierId, ModifierTypeId}
import io.circe.Encoder
import io.circe.syntax._
import scorex.crypto.encode.Base16
import scala.util.Try

case class EncryBlock(override val header: EncryBlockHeader,
                      override val payload: EncryBlockPayload,
                      adProofsOpt: Option[ADProofs]) extends EncryBaseBlock with ModifierValidator {

  override type M = EncryBlock

  override val toSeq: Seq[EncryPersistentModifier] = Seq(header, payload) ++ adProofsOpt.toSeq

  override def transactions: Seq[BaseTransaction] = payload.transactions

  override def semanticValidity: Try[Unit] = validateSemantically.toTry

  def validateSemantically: ValidationResult =
    accumulateErrors
      .demand(header.transactionsRoot != payload.digest, "Invalid payload root hash")
      .result

  override def parentId: ModifierId = header.parentId

  override val modifierTypeId: ModifierTypeId = EncryBlock.modifierTypeId

  override lazy val id: ModifierId = header.id

  override def serializer: Serializer[EncryBlock] = EncryBlockSerializer

  def dataString: String = {
    val encodedId: String = Base16.encode(id)
    val encodedParentId: String = Base16.encode(parentId)
    val proofsRoot: String = Base16.encode(header.adProofsRoot)
    val stateRoot: String = Base16.encode(header.stateRoot)
    val transactionsRoot: String = Base16.encode(header.transactionsRoot)
    val proofs: String = adProofsOpt.map(p => Base16.encode(p.bytes)).getOrElse("")
    val solution: String = header.equihashSolution.ints.mkString("{", ", ", "}")
    val (minerAddress: String, minerReward: Long) = minerInfo(payload.transactions.last)
    val feesTotal: Long = payload.transactions.map(_.fee).sum
    val txsSize: Int = payload.transactions.map(_.bytes.length).sum

    s"('$encodedId', '$encodedParentId', '${header.version}', '${header.height}', '$proofsRoot', '$stateRoot', " +
      s"'$transactionsRoot', '${header.timestamp}', '${header.difficulty}', '${bytes.length}', '$solution', '$proofs', " +
      s"'${payload.transactions.size}', '$minerAddress', '$minerReward', '$feesTotal', '$txsSize', TRUE)"
  }

  private def minerInfo(coinbase: BaseTransaction): (String, Long) = coinbase.directives.head match {
    case TransferDirective(address, amount, tokenIdOpt) if tokenIdOpt.isEmpty => address -> amount
    case _ => "unknown" -> 0
  }
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
    var pointer: Int = 4
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
