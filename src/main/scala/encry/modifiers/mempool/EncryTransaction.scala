package encry.modifiers.mempool

import com.google.common.primitives.{Bytes, Longs, Shorts}
import encry.ModifierId
import encry.modifiers.mempool.directive.{Directive, DirectiveSerializer}
import encry.modifiers.serialization.Serializer
import encry.modifiers.state.box.Box.Amount
import encry.settings.Algos
import encry.validation.{ModifierValidator, ValidationResult}
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import org.encryfoundation.common.transaction.{Input, InputSerializer, Proof, ProofSerializer}
import org.encryfoundation.prismlang.core.Types
import org.encryfoundation.prismlang.core.wrapped.{PObject, PValue}
import scorex.crypto.hash.Digest32
import scala.util.Try

case class EncryTransaction(fee: Amount,
                            timestamp: Long,
                            inputs: IndexedSeq[Input],
                            directives: IndexedSeq[Directive],
                            defaultProofOpt: Option[Proof])
  extends Transaction with ModifierValidator {

  override type M = EncryTransaction

  override lazy val serializer: Serializer[M] = EncryTransactionSerializer

  override val messageToSign: Array[Byte] =
    UnsignedEncryTransaction.bytesToSign(fee, timestamp, inputs, directives)

  val id: ModifierId = ModifierId !@@ Algos.hash(messageToSign)
  override lazy val semanticValidity: Try[Unit] = validateStateless.toTry

  def validSize: Boolean = length <= maxSize

  def validateStateless: ValidationResult =
    accumulateErrors
      .demand(validSize, "Invalid size")
      .demand(fee >= 0, "Negative fee amount")
      .demand(directives.forall(_.isValid), "Invalid outputs")
      .demand(inputs.size <= Short.MaxValue, s"Too many inputs in transaction $toString")
      .demand(directives.size <= Short.MaxValue, s"Too many directives in transaction $toString")
      .result

  override val tpe: Types.Product = Types.EncryTransaction

  override def asVal: PValue = PValue(PObject(Map(
    "inputs" -> PValue(inputs.map(_.boxId.toList), Types.PCollection(Types.PCollection.ofByte)),
    "outputs" -> PValue(newBoxes.map(_.asPrism), Types.PCollection(Types.EncryBox)),
    "messageToSign" -> PValue(messageToSign, Types.PCollection.ofByte)
  ), tpe), tpe)
}

object EncryTransaction {

  implicit val jsonEncoder: Encoder[EncryTransaction] = (tx: EncryTransaction) => Map(
    "id" -> Algos.encode(tx.id).asJson,
    "fee" -> tx.fee.asJson,
    "timestamp" -> tx.timestamp.asJson,
    "inputs" -> tx.inputs.map(_.asJson).asJson,
    "directives" -> tx.directives.map(_.asJson).asJson,
    "outputs" -> tx.newBoxes.toSeq.map(_.asJson).asJson,
    "defaultProofOpt" -> tx.defaultProofOpt.map(_.asJson).asJson
  ).asJson

  implicit val jsonDecoder: Decoder[EncryTransaction] = (c: HCursor) => {
    for {
      fee <- c.downField("fee").as[Long]
      timestamp <- c.downField("timestamp").as[Long]
      inputs <- c.downField("inputs").as[IndexedSeq[Input]]
      directives <- c.downField("directives").as[IndexedSeq[Directive]]
      defaultProofOpt <- c.downField("defaultProofOpt").as[Option[Proof]]
    } yield {
      EncryTransaction(
        fee,
        timestamp,
        inputs,
        directives,
        defaultProofOpt
      )
    }
  }
}

object EncryTransactionSerializer extends Serializer[EncryTransaction] {

  case object SerializationException extends Exception("Serialization failed.")

  override def toBytes(obj: EncryTransaction): Array[Byte] = {
    Bytes.concat(
      Longs.toByteArray(obj.fee),
      Longs.toByteArray(obj.timestamp),
      Shorts.toByteArray(obj.inputs.size.toShort),
      Shorts.toByteArray(obj.directives.size.toShort),
      obj.inputs.map(u => Shorts.toByteArray(u.bytes.length.toShort) ++ u.bytes).foldLeft(Array[Byte]())(_ ++ _),
      obj.directives.map { d =>
        val bytes: Array[Byte] = DirectiveSerializer.toBytes(d)
        Shorts.toByteArray(bytes.length.toShort) ++ bytes
      }.reduceLeft(_ ++ _),
      obj.defaultProofOpt.map(p => ProofSerializer.toBytes(p)).getOrElse(Array.empty)
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[EncryTransaction] = Try {

    val fee: Amount = Longs.fromByteArray(bytes.take(8))
    val timestamp: Amount = Longs.fromByteArray(bytes.slice(8, 16))
    val unlockersQty: Int = Shorts.fromByteArray(bytes.slice(16, 18))
    val directivesQty: Int = Shorts.fromByteArray(bytes.slice(18, 20))
    val leftBytes1: Array[Byte] = bytes.drop(20)
    val (unlockers: IndexedSeq[Input], unlockersLen: Int) = (0 until unlockersQty)
      .foldLeft(IndexedSeq[Input](), 0) { case ((acc, shift), _) =>
        val len: Int = Shorts.fromByteArray(leftBytes1.slice(shift, shift + 2))
        InputSerializer.parseBytes(leftBytes1.slice(shift + 2, shift + 2 + len))
          .map(u => (acc :+ u, shift + 2 + len))
          .getOrElse(throw SerializationException)
      }
    val leftBytes2: Array[Byte] = leftBytes1.drop(unlockersLen)
    val (directives: IndexedSeq[Directive], directivesLen: Int) = (0 until directivesQty)
      .foldLeft(IndexedSeq[Directive](), 0) { case ((acc, shift), _) =>
        val len: Int = Shorts.fromByteArray(leftBytes2.slice(shift, shift + 2))
        DirectiveSerializer.parseBytes(leftBytes2.slice(shift + 2, shift + 2 + len))
          .map(d => (acc :+ d, shift + 2 + len))
          .getOrElse(throw SerializationException)
      }
    val proofOpt: Option[Proof] = if (leftBytes2.length - directivesLen == 0) None else {
      ProofSerializer.parseBytes(leftBytes2.drop(directivesLen)).map(Some(_))
        .getOrElse(throw SerializationException)
    }

    EncryTransaction(fee, timestamp, unlockers, directives, proofOpt)
  }
}

/** Unsigned version of EncryTransaction (without any
  * proofs for which interactive message is required) */
case class UnsignedEncryTransaction(fee: Amount,
                                    timestamp: Long,
                                    inputs: IndexedSeq[Input],
                                    directives: IndexedSeq[Directive]) {

  val messageToSign: Array[Byte] =
    UnsignedEncryTransaction.bytesToSign(fee, timestamp, inputs, directives)

  def toSigned(proofs: IndexedSeq[Seq[Proof]], defaultProofOpt: Option[Proof]): EncryTransaction = {
    val signedInputs: IndexedSeq[Input] = inputs.zipWithIndex.map { case (input, idx) =>
      if (proofs.nonEmpty && proofs.lengthCompare(idx + 1) <= 0) input.copy(proofs = proofs(idx).toList) else input
    }
    EncryTransaction(fee, timestamp, signedInputs, directives, defaultProofOpt)
  }
}

object UnsignedEncryTransaction {

  def bytesToSign(fee: Amount,
                  timestamp: Long,
                  unlockers: IndexedSeq[Input],
                  directives: IndexedSeq[Directive]): Digest32 =
    Algos.hash(Bytes.concat(
      unlockers.map(_.bytesWithoutProof).foldLeft(Array[Byte]())(_ ++ _),
      directives.map(_.bytes).foldLeft(Array[Byte]())(_ ++ _),
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee)
    ))
}
