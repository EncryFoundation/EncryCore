package encry.modifiers.mempool

import com.google.common.primitives.{Bytes, Longs, Shorts}
import encry.modifiers.mempool.EncryBaseTransaction.TransactionValidationException
import encry.modifiers.mempool.directive.{CoinbaseDirective, Directive, DirectiveSerializer}
import encry.modifiers.state.box.AssetBox
import encry.modifiers.state.box.proof.{Proof, ProofSerializer}
import encry.modifiers.state.box.proposition.OpenProposition
import encry.settings.{Algos, Constants}
import encry.utils.Utils
import encrywm.lang.backend.env.{ESObject, ESValue}
import encrywm.lib.Types
import encrywm.lib.Types.{ESByteVector, ESList, ESLong, ESTransaction}
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
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

  override val maxSize: Int = EncryTransaction.MaxSize

  override val feeBox: Option[AssetBox] =
    if (fee > 0) Some(AssetBox(OpenProposition, Utils.nonceFromDigest(Algos.hash(txHash)), fee))
    else None

  override lazy val serializer: Serializer[M] = EncryTransactionSerializer

  override lazy val txHash: Digest32 =
    EncryTransaction.getHash(fee, timestamp, unlockers, directives)

  override lazy val isCoinbase: Boolean = directives.head.isInstanceOf[CoinbaseDirective]

  override lazy val semanticValidity: Try[Unit] =
    Try(directives.map(_.idx).foldLeft(-1)((a, b) => if (b > a) b else throw TransactionValidationException("Invalid order")))
      .flatMap { _ =>
        if (!validSize) {
          Failure(TransactionValidationException("Invalid size"))
        } else if (fee < 0) {
          Failure(TransactionValidationException("Negative fee"))
        } else if (!directives.forall(_.isValid)) {
          Failure(TransactionValidationException("Bad outputs"))
        } else Success()
      }

  override val esType: Types.ESProduct = ESTransaction

  override def asVal: ESValue = ESValue(ESTransaction.ident.toLowerCase, ESTransaction)(convert)

  override def convert: ESObject = {
    val fields = Map(
      "fee" -> ESValue("fee", ESLong)(fee),
      "messageToSign" -> ESValue("messageToSign", ESByteVector)(txHash),
      "outputs" -> ESValue("outputs", ESList(Types.ESBox))(newBoxes.map(_.convert).toList),
      "unlockers" -> ESValue("unlockers", ESList(Types.ESUnlocker))(unlockers.map(_.convert).toList),
      "timestamp" -> ESValue("timestamp", ESLong)(timestamp)
    )
    ESObject(Types.ESTransaction.ident, fields, esType)
  }
}

object EncryTransaction {

  val MaxSize: Int = Constants.TransactionMaxSize

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
              directives: IndexedSeq[Directive]): Digest32 = Algos.hash(
    Bytes.concat(
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

  def estimateMinimalFee(unlockers: IndexedSeq[Unlocker],
                         directives: IndexedSeq[Directive],
                         defaultProofOpt: Option[Proof]): Amount = {
      val length: Int =
        unlockers.map(_.bytes.length).sum +
        directives.map(_.bytes.length).sum +
        (unlockers.size * 2) +
        (directives.size * 2) +
        defaultProofOpt.map(_.bytes.length).getOrElse(0) +
        20
      Constants.FeeMinAmount + directives.map(_.cost).sum + (Constants.PersistentByteCost * length)
    }
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
