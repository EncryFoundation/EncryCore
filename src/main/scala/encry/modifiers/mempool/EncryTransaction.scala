package encry.modifiers.mempool

import com.google.common.primitives.{Bytes, Ints, Longs}
import encry.crypto.PublicKey25519
import encry.modifiers.mempool.EncryBaseTransaction.TxTypeId
import encry.modifiers.mempool.directive.{Directive, DirectiveSerializer}
import encry.modifiers.state.box.proof.Signature25519
import encry.modifiers.state.box.proposition.HeightProposition
import encry.modifiers.state.box.{EncryBaseBox, OpenBox}
import encry.settings.Algos
import encry.utils.Utils
import encry.view.history.Height
import io.circe.Json
import io.circe.syntax._
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.Box.Amount
import scorex.crypto.hash.Digest32
import scorex.crypto.signatures.{PublicKey, Signature}

import scala.util.{Failure, Success, Try}

case class EncryTransaction(override val accountPubKey: PublicKey25519,
                            override val fee: Amount,
                            override val timestamp: Long,
                            override val signature: Signature25519,
                            override val unlockers: IndexedSeq[Unlocker],
                            directives: IndexedSeq[Directive]) extends EncryBaseTransaction {

  override type M = EncryTransaction

  override val length: Int = 120 + (32 * unlockers.size) + (45 * directives.size)

  override val maxSize: Int = PaymentTransaction.maxSize

  override val typeId: TxTypeId = PaymentTransaction.typeId

  override val feeBox: Option[OpenBox] =
    Some(OpenBox(HeightProposition(Height @@ 0), Utils.nonceFromDigest(Algos.hash(txHash)), fee))

  override val newBoxes: Traversable[EncryBaseBox] =
    Seq(feeBox.get) ++ directives.flatMap(_.boxes(txHash))

  override lazy val serializer: Serializer[M] = EncryTransactionSerializer

  override def json: Json = Map(
    "id" -> Algos.encode(id).asJson,
    "proposition" -> account.toString.asJson,
    "fee" -> fee.asJson,
    "timestamp" -> timestamp.asJson,
    "signature" -> Algos.encode(signature.signature).asJson,
    "inputs" -> unlockers.map(_.json).asJson,
    "directives" -> directives.map(_.json).asJson
  ).asJson

  override lazy val txHash: Digest32 =
    EncryTransaction.getHash(accountPubKey, fee, timestamp, unlockers, directives)

  override lazy val semanticValidity: Try[Unit] = {
    if (!validSize) {
      Failure(new Error("Invalid size"))
    } else if (!validSignature) {
      Failure(new Error("Invalid signature"))
    } else if (!directives.forall(_.isValid)) {
      Failure(new Error("Bad outputs"))
    } else if (directives.map(_.idx).toSet.size != directives.size) {
      Failure(new Error("Non-unique directive indexes"))
    } else if (fee < minimalFee) {
      Failure(new Error("Fee amount too small"))
    } else Success()
  }
}

object EncryTransaction {

  val MaxSize: Int = 350

  val TypeId: TxTypeId = 8.toByte

  def getHash(accountPubKey: PublicKey25519,
              fee: Amount,
              timestamp: Long,
              unlockers: IndexedSeq[Unlocker],
              directives: IndexedSeq[Directive]): Digest32 = Algos.hash(
    Bytes.concat(
      Array[Byte](TypeId),
      accountPubKey.pubKeyBytes,
      unlockers.map(_.bytes).foldLeft(Array[Byte]())(_ ++ _),
      directives.map(_.bytes).foldLeft(Array[Byte]())(_ ++ _),
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee)
    )
  )

  def getMessageToSign(accountPubKey: PublicKey25519,
                       fee: Amount,
                       timestamp: Long,
                       unlockers: IndexedSeq[Unlocker],
                       directives: IndexedSeq[Directive]): Array[Byte] =
    getHash(accountPubKey, fee, timestamp, unlockers, directives)
}

object EncryTransactionSerializer extends Serializer[EncryTransaction] {

  override def toBytes(obj: EncryTransaction): Array[Byte] = {
    Bytes.concat(
      obj.accountPubKey.pubKeyBytes,
      Longs.toByteArray(obj.fee),
      Longs.toByteArray(obj.timestamp),
      obj.signature.signature,
      Ints.toByteArray(obj.unlockers.size),
      Ints.toByteArray(obj.directives.size),
      obj.unlockers.map(u => Ints.toByteArray(u.bytes.length) ++ u.bytes)
        .foldLeft(Array[Byte]())(_ ++ _),
      obj.directives.map(d => Ints.toByteArray(d.bytes.length) ++ DirectiveSerializer.toBytes(d))
        .foldLeft(Array[Byte]())(_ ++ _)
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[EncryTransaction] = Try {

    val accPubKey = PublicKey25519(PublicKey @@ bytes.slice(0, 32))
    val fee = Longs.fromByteArray(bytes.slice(32, 40))
    val timestamp = Longs.fromByteArray(bytes.slice(40, 48))
    val signature = Signature25519(Signature @@ bytes.slice(48, 112))
    val unlockersQty = Ints.fromByteArray(bytes.slice(112, 116))
    val directivesQty = Ints.fromByteArray(bytes.slice(116, 120))
    val leftBytes1 = bytes.drop(120)
    val (unlockers, unlockersLen) = (0 until unlockersQty).foldLeft(IndexedSeq[Unlocker](), 0) { case ((acc, shift), _) =>
      val len = Ints.fromByteArray(leftBytes1.slice(shift, shift + 4))
      UnlockerSerializer.parseBytes(leftBytes1.slice(shift + 4, shift + 4 + len)).map(u => (acc :+ u, shift + len))
        .getOrElse(throw new Exception("Serialization failed."))
    }
    val leftBytes2 = leftBytes1.drop(unlockersLen)
    val directives = (0 until directivesQty).foldLeft(IndexedSeq[Directive](), leftBytes2) { case ((acc, bs), _) =>
      val len = Ints.fromByteArray(bs.take(4))
      DirectiveSerializer.parseBytes(bs.slice(5, 5 + len)).map(d => (acc :+ d, bs.drop(5 + len)))
        .getOrElse(throw new Exception("Serialization failed."))
    }._1

    EncryTransaction(accPubKey, fee, timestamp, signature, unlockers, directives)
  }
}
