package encry.modifiers.mempool

import com.google.common.primitives.{Bytes, Longs, Shorts}
import encry.modifiers.mempool.EncryBaseTransaction.TransactionValidationException
import encry.modifiers.mempool.directive.{Directive, DirectiveSerializer}
import encry.settings.{Algos, Constants}
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import org.encryfoundation.prismlang.core.Types
import org.encryfoundation.prismlang.core.wrapped.{PObject, PValue}
import scorex.core.serialization.{SerializationException, Serializer}
import scorex.core.transaction.box.Box.Amount
import scorex.crypto.hash.Digest32

import scala.util.{Failure, Success, Try}

/** Transaction is an atomic state modifier. */
case class EncryTransaction(override val fee: Amount,
                            override val timestamp: Long,
                            override val unlockers: IndexedSeq[Unlocker],
                            override val directives: IndexedSeq[Directive],
                            override val defaultProofOpt: Option[Proof]) extends EncryBaseTransaction {

  override type M = EncryTransaction

  override lazy val length: Int = this.bytes.length

  override val maxSize: Int = Constants.TransactionMaxSize

  override lazy val serializer: Serializer[M] = EncryTransactionSerializer

  override lazy val txHash: Digest32 =
    EncryTransaction.getHash(fee, timestamp, unlockers, directives)

  override lazy val semanticValidity: Try[Unit] =
    Try(directives.map(_.idx).foldLeft(-1)((a, b) => if (b > a) b else throw TransactionValidationException("Invalid order")))
      .flatMap { _ =>
        if (!validSize) Failure(TransactionValidationException("Invalid size"))
        else if (fee < 0) Failure(TransactionValidationException("Negative fee"))
        else if (!directives.forall(_.isValid)) Failure(TransactionValidationException("Bad outputs"))
        else Success()
      }

  override val esType: Types.Product = Types.EncryTransaction

  override def asVal: PValue = PValue(convert, esType)

  override def convert: PObject = {
    val fields: Map[String, PValue] = Map(
      "inputs" -> PValue(unlockers.map(_.boxId), Types.PCollection(Types.PCollection.ofByte)),
      "outputs" -> PValue(directives.flatMap(_.boxes(txHash)), Types.PCollection(Types.EncryBox))
    )
    PObject(fields, esType)
  }
}

object EncryTransaction {

  implicit val jsonEncoder: Encoder[EncryTransaction] = (tx: EncryTransaction) => Map(
    "id" -> Algos.encode(tx.id).asJson,
    "fee" -> tx.fee.asJson,
    "timestamp" -> tx.timestamp.asJson,
    "unlockers" -> tx.unlockers.map(_.asJson).asJson,
    "directives" -> tx.directives.map(_.asJson).asJson,
    "defaultProofOpt" -> tx.defaultProofOpt.map(_.asJson).asJson
  ).asJson

  implicit val jsonDecoder: Decoder[EncryTransaction] = (c: HCursor) => {
    for {
      fee <- c.downField("fee").as[Long]
      timestamp <- c.downField("timestamp").as[Long]
      unlockers <- c.downField("unlockers").as[IndexedSeq[Unlocker]]
      directives <- c.downField("directives").as[IndexedSeq[Directive]]
      defaultProofOpt <- c.downField("defaultProofOpt").as[Option[Proof]]
    } yield {
      EncryTransaction(
        fee,
        timestamp,
        unlockers,
        directives,
        defaultProofOpt
      )
    }
  }

  def getHash(fee: Amount,
              timestamp: Long,
              unlockers: IndexedSeq[Unlocker],
              directives: IndexedSeq[Directive]): Digest32 =
    Algos.hash(Bytes.concat(
      unlockers.map(_.bytesWithoutProof).foldLeft(Array[Byte]())(_ ++ _),
      directives.map(_.bytes).foldLeft(Array[Byte]())(_ ++ _),
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee)
    )
  )

  def getMessageToSign(fee: Amount,
                       timestamp: Long,
                       unlockers: IndexedSeq[Unlocker],
                       directives: IndexedSeq[Directive]): Array[Byte] =
    getHash(fee, timestamp, unlockers, directives)
}

object EncryTransactionSerializer extends Serializer[EncryTransaction] {

  override def toBytes(obj: EncryTransaction): Array[Byte] = {
    Bytes.concat(
      Longs.toByteArray(obj.fee),
      Longs.toByteArray(obj.timestamp),
      Shorts.toByteArray(obj.unlockers.size.toShort),
      Shorts.toByteArray(obj.directives.size.toShort),
      obj.unlockers.map(u => Shorts.toByteArray(u.bytes.length.toShort) ++ u.bytes).foldLeft(Array[Byte]())(_ ++ _),
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
    val (unlockers: IndexedSeq[Unlocker], unlockersLen: Int) = (0 until unlockersQty)
      .foldLeft(IndexedSeq[Unlocker](), 0) { case ((acc, shift), _) =>
        val len: Int = Shorts.fromByteArray(leftBytes1.slice(shift, shift + 2))
        UnlockerSerializer.parseBytes(leftBytes1.slice(shift + 2, shift + 2 + len))
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
