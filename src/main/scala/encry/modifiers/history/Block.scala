package encry.modifiers.history

import BlockProto.BlockProtoMessage
import com.google.common.primitives.{Bytes, Ints}
import encry.modifiers.mempool.Transaction
import encry.modifiers.mempool.directive.TransferDirective
import encry.modifiers.state.box.EncryProposition
import encry.modifiers.{EncryPersistentModifier, TransactionsCarryingPersistentNodeViewModifier}
import encry.utils.CoreTaggedTypes.{ModifierId, ModifierTypeId}
import encry.validation.{ModifierValidator, ValidationResult}
import io.circe.{Decoder, Encoder, HCursor}
import io.circe.syntax._
import org.encryfoundation.common.serialization.Serializer
import scorex.crypto.encode.Base16

import scala.util.Try

case class Block(header: Header,
                 payload: Payload,
                 adProofsOpt: Option[ADProofs])
  extends TransactionsCarryingPersistentNodeViewModifier[EncryProposition, Transaction]
    with EncryPersistentModifier with ModifierValidator {

  override type M = Block

  val toSeq: Seq[EncryPersistentModifier] = Seq(header, payload) ++ adProofsOpt.toSeq

  override def transactions: Seq[Transaction] = payload.transactions

  def semanticValidity: Try[Unit] = validateSemantically.toTry

  def validateSemantically: ValidationResult =
    accumulateErrors
      .demand(header.transactionsRoot != payload.digest, "Invalid payload root hash")
      .result

  override def parentId: ModifierId = header.parentId

  override val modifierTypeId: ModifierTypeId = Block.modifierTypeId

  override lazy val id: ModifierId = header.id

  override def serializer: Serializer[Block] = BlockSerializer

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

  private def minerInfo(coinbase: Transaction): (String, Long) = coinbase.directives.head match {
    case TransferDirective(address, amount, tokenIdOpt) if tokenIdOpt.isEmpty => address -> amount
    case _ => "unknown" -> 0
  }

  override def toString: String = s"<Block height=${header.height} timestamp=${header.timestamp} " +
    s"txQty=${payload.transactions.size} id=${header.encodedId}>"

}

object Block {

  type Timestamp = Long
  type Version = Byte
  type Height = Int

  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (100: Byte)

  implicit val jsonEncoder: Encoder[Block] = (b: Block) => Map(
    "header"   -> b.header.asJson,
    "payload"  -> b.payload.asJson,
    "adProofs" -> b.adProofsOpt.map(_.asJson).getOrElse(Map.empty[String, String].asJson)
  ).asJson

  implicit val jsonDecoder: Decoder[Block] = (c: HCursor) => {
    for {
      header  <- c.downField("header").as[Header]
      payload <- c.downField("payload").as[Payload]
    } yield Block(
      header,
      payload,
      None
    )
  }
}

object BlockSerializer extends Serializer[Block] {

  def toProto(block: Block): BlockProtoMessage = BlockProtoMessage()
    .withHeader(HeaderSerializer.toProto(block.header))
    .withPayload(PayloadSerializer.toProto(block.payload))
    .withAdProofsOpt(ADProofSerializer.toProto(block.adProofsOpt.get))

  def fromProto(message: BlockProtoMessage): Block = Block(
    message.header.map(x => HeaderSerializer.fromProto(x)).get,
    message.payload.map(x => PayloadSerializer.fromProto(x)).get,
    message.adProofsOpt.map(x => ADProofSerializer.fromProto(x))
  )

  override def toBytes(obj: Block): Array[Byte] = {

    val headerBytes: Array[Byte] = obj.header.serializer.toBytes(obj.header)
    val payloadBytes: Array[Byte] = obj.payload.serializer.toBytes(obj.payload)
    val aDProofsBytes: Array[Byte] =
      obj.adProofsOpt.map(_.serializer.toBytes(obj.adProofsOpt.get)).getOrElse(Array.emptyByteArray)
    Bytes.concat(
      Ints.toByteArray(headerBytes.length),
      headerBytes,
      Ints.toByteArray(payloadBytes.length),
      payloadBytes,
      Ints.toByteArray(aDProofsBytes.length),
      aDProofsBytes
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[Block] = Try {
    var pointer: Int = 4
    val headerSize: Int = Ints.fromByteArray(bytes.slice(0, pointer))
    val header: Try[Header] = HeaderSerializer.parseBytes(bytes.slice(pointer, pointer + headerSize))
    pointer += headerSize
    val payloadSize: Int = Ints.fromByteArray(bytes.slice(pointer, pointer + 4))
    val payload: Try[Payload] =
      PayloadSerializer.parseBytes(bytes.slice(pointer + 4, pointer + 4 + payloadSize))
    pointer += payloadSize + 4
    val aDProofsSize: Int = Ints.fromByteArray(bytes.slice(pointer, pointer + 4))
    val aDProofs: Try[ADProofs] = ADProofSerializer.parseBytes(bytes.slice(pointer + 4, pointer + 4 + aDProofsSize))
    new Block(header.get, payload.get, Option(aDProofs.get))
  }
}