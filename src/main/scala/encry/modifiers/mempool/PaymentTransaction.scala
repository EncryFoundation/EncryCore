package encry.modifiers.mempool

import com.google.common.primitives.{Bytes, Ints, Longs}
import encry.crypto.{PublicKey25519, Signature25519}
import encry.modifiers.mempool.EncryTransaction._
import encry.modifiers.mempool.directive.{Directive, DirectiveDeserializer}
import encry.modifiers.state.box.proposition.HeightProposition
import encry.modifiers.state.box.{EncryBaseBox, OpenBox}
import encry.settings.Algos
import encry.utils.Utils
import encry.view.history.Height
import io.circe.Json
import io.circe.syntax._
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.Box.Amount
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Digest32
import scorex.crypto.signatures.{PublicKey, Signature}

import scala.util.{Failure, Success, Try}

case class PaymentTransaction(override val accountPubKey: PublicKey25519,
                              override val fee: Amount,
                              override val timestamp: Long,
                              override val signature: Signature25519,
                              override val useBoxes: IndexedSeq[ADKey],
                              directives: IndexedSeq[Directive])
  extends EncryTransaction {

  override type M = PaymentTransaction

  override val length: Int = 120 + (32 * useBoxes.size) + (45 * directives.size)

  override val maxSize: Int = PaymentTransaction.maxSize

  override val typeId: TxTypeId = PaymentTransaction.typeId

  override val feeBox: Option[OpenBox] =
    Some(OpenBox(HeightProposition(Height @@ 0), Utils.nonceFromDigest(Algos.hash(txHash)), fee))

  override val newBoxes: Traversable[EncryBaseBox] =
    Seq(feeBox.get) ++ directives.flatMap(_.boxes(txHash))

  override lazy val serializer: Serializer[M] = PaymentTransactionSerializer

  override def json: Json = Map(
    "id" -> Algos.encode(id).asJson,
    "proposition" -> account.toString.asJson,
    "fee" -> fee.asJson,
    "timestamp" -> timestamp.asJson,
    "signature" -> Algos.encode(signature.signature).asJson,
    "inputs" -> useBoxes.map { id =>
      Map(
        "id" -> Algos.encode(id).asJson,
      ).asJson
    }.asJson,
    "directives" -> directives.map(_.json).asJson
  ).asJson

  override lazy val txHash: Digest32 =
    PaymentTransaction.getHash(accountPubKey, fee, timestamp, useBoxes, directives)

  override lazy val semanticValidity: Try[Unit] = {
    if (!validSize) {
      Failure(new Error("Invalid size"))
    } else if (!validSignature) {
      Failure(new Error("Invalid signature"))
    } else if (!directives.forall(_.isValid)) {
      Failure(new Error("Bad outputs"))
    } else if (fee < minimalFee) {
      Failure(new Error("Fee amount too small"))
    } else Success()
  }
}

object PaymentTransaction {

  val maxSize: Int = 350

  val typeId: TxTypeId = 1.toByte

  def getHash(accountPubKey: PublicKey25519,
              fee: Amount,
              timestamp: Long,
              useBoxes: IndexedSeq[ADKey],
              directives: IndexedSeq[Directive]): Digest32 = Algos.hash(
    Bytes.concat(
      Array[Byte](typeId),
      accountPubKey.pubKeyBytes,
      useBoxes.foldLeft(Array[Byte]())(_ ++ _),
      directives.map(_.bytes).foldLeft(Array[Byte]())(_ ++ _),
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee)
    )
  )

  def getMessageToSign(accountPubKey: PublicKey25519,
                       fee: Amount,
                       timestamp: Long,
                       useBoxes: IndexedSeq[ADKey],
                       directives: IndexedSeq[Directive]): Array[Byte] =
    getHash(accountPubKey, fee, timestamp, useBoxes, directives)
}

object PaymentTransactionSerializer extends Serializer[PaymentTransaction] {

  override def toBytes(obj: PaymentTransaction): Array[Byte] = {
    Bytes.concat(
      obj.accountPubKey.pubKeyBytes,
      Longs.toByteArray(obj.fee),
      Longs.toByteArray(obj.timestamp),
      obj.signature.signature,
      Ints.toByteArray(obj.useBoxes.length),
      Ints.toByteArray(obj.directives.length),
      obj.useBoxes.foldLeft(Array[Byte]())((a, b) => Bytes.concat(a, b)),
      obj.directives.map(d => Ints.toByteArray(d.bytes.length) ++ (d.typeId +: d.bytes)).foldLeft(Array[Byte]())(_ ++ _)
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[PaymentTransaction] = Try {

    val accPubKey = PublicKey25519(PublicKey @@ bytes.slice(0, 32))
    val fee = Longs.fromByteArray(bytes.slice(32, 40))
    val timestamp = Longs.fromByteArray(bytes.slice(40, 48))
    val signature = Signature25519(Signature @@ bytes.slice(48, 112))
    val inputsQty = Ints.fromByteArray(bytes.slice(112, 116))
    val directivesQty = Ints.fromByteArray(bytes.slice(116, 120))
    val s = 120
    val outElementLength = 32
    val useOutputs = (0 until inputsQty).map { i =>
      ADKey @@ bytes.slice(s + (i * outElementLength), s + (i + 1) * outElementLength)
    }
    val s2 = s + (inputsQty * outElementLength)
    val leftBytes = bytes.drop(s2)
    val directives = (0 until directivesQty).foldLeft(Seq[Directive](), leftBytes) { case ((acc, bs), _) =>
        val len = Ints.fromByteArray(bs.take(4))
        DirectiveDeserializer.parseBytes(bs.slice(5, 5 + len), bs(5)).map(d => (acc :+ d, bs.drop(5 + len)))
          .getOrElse(throw new Exception("Serialization failed."))
      }._1.toIndexedSeq

    PaymentTransaction(accPubKey, fee, timestamp, signature, useOutputs, directives)
  }
}
