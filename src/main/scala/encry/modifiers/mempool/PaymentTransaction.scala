package encry.modifiers.mempool

import com.google.common.primitives.{Bytes, Ints, Longs}
import encry.account.Address
import encry.modifiers.mempool.EncryTransaction._
import encry.modifiers.state.box.proposition.{AddressProposition, HeightProposition}
import encry.modifiers.state.box.{AssetBox, EncryBaseBox, OpenBox}
import encry.settings.Algos
import encry.view.history.Height
import io.circe.Json
import io.circe.syntax._
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.Box.Amount
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.crypto.authds.ADKey
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Digest32
import scorex.crypto.signatures.{PublicKey, Signature}

import scala.util.{Success, Try}

case class PaymentTransaction(override val proposition: PublicKey25519Proposition,
                              override val fee: Amount,
                              override val timestamp: Long,
                              override val signature: Signature25519,
                              override val useBoxes: IndexedSeq[ADKey],
                              createBoxes: IndexedSeq[(Address, Amount)])
  extends EncryTransaction {

  override type M = PaymentTransaction

  override val length: Int = 120 + (32 * useBoxes.size) + (45 * createBoxes.size)

  // Type of actual Tx type.
  override val typeId: TxTypeId = PaymentTransaction.typeId

  override val feeBox: Option[OpenBox] =
    Some(OpenBox(HeightProposition(Height @@ 0), nonceFromDigest(Algos.hash(txHash)), fee))

  override val newBoxes: Traversable[EncryBaseBox] =
    Seq(feeBox.get) ++
      createBoxes.zipWithIndex.map { case ((addr, amount), idx) =>
        val nonce = nonceFromDigest(Algos.hash(txHash ++ Ints.toByteArray(idx)))
        AssetBox(AddressProposition(addr), nonce, amount)
      }

  override def serializer: Serializer[M] = PaymentTransactionSerializer

  override def json: Json = Map(
    "type" -> "Payment".asJson,
    "id" -> Base58.encode(id).asJson,
    "inputs" -> useBoxes.map { id =>
      Map(
        "id" -> Algos.encode(id).asJson,
        "signature" -> Base58.encode(signature.bytes).asJson
      ).asJson
    }.asJson,
    "outputs" -> createBoxes.map { case (_, amount) =>
      Map(
        "script" -> "".asJson,
        "amount" -> amount.asJson
      ).asJson
    }.asJson
  ).asJson

  override lazy val txHash: Digest32 =
    PaymentTransaction.getHash(proposition, fee, timestamp, useBoxes, createBoxes)

  override lazy val semanticValidity: Try[Unit] = {
    // Signature validity checks.
    if (!validSignature) {
      throw new Error("Invalid signature")
    }
    // `Amount` & `Address` validity checks.
    if (!createBoxes.forall { i =>
      i._2 > 0 && AddressProposition.validAddress(i._1)
    }) {
      throw new Error("Bad outputs")
    }
    // `Fee` amount check.
    if (fee < minimalFee)
      throw new Error("Fee amount too small")

    Success()
  }
}

object PaymentTransaction {

  val typeId: TxTypeId = 1.toByte

  def getHash(proposition: PublicKey25519Proposition,
              fee: Amount,
              timestamp: Long,
              useBoxes: IndexedSeq[ADKey],
              createBoxes: IndexedSeq[(Address, Amount)]): Digest32 = Algos.hash(
    Bytes.concat(
      Array[Byte](typeId),
      proposition.pubKeyBytes,
      scorex.core.utils.concatFixLengthBytes(useBoxes),
      scorex.core.utils.concatFixLengthBytes(createBoxes.map { case (addr, amount) =>
        AddressProposition.getAddrBytes(addr) ++ Longs.toByteArray(amount)
      }),
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee)
    )
  )

  def getMessageToSign(proposition: PublicKey25519Proposition,
                       fee: Amount,
                       timestamp: Long,
                       useBoxes: IndexedSeq[ADKey],
                       createBoxes: IndexedSeq[(Address, Amount)]): Array[Byte] =
    getHash(proposition, fee, timestamp, useBoxes, createBoxes)
}

object PaymentTransactionSerializer extends Serializer[PaymentTransaction] {

  override def toBytes(obj: PaymentTransaction): Array[Byte] = {
    Bytes.concat(
      obj.proposition.pubKeyBytes,
      Longs.toByteArray(obj.fee),
      Longs.toByteArray(obj.timestamp),
      obj.signature.signature,
      Ints.toByteArray(obj.useBoxes.length),
      Ints.toByteArray(obj.createBoxes.length),
      obj.useBoxes.foldLeft(Array[Byte]())((a, b) => Bytes.concat(a, b)),
      obj.createBoxes.foldLeft(Array[Byte]())((a, b) =>
        Bytes.concat(a, AddressProposition.getAddrBytes(b._1), Longs.toByteArray(b._2)))
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[PaymentTransaction] = Try {

    val sender = new PublicKey25519Proposition(PublicKey @@ bytes.slice(0, 32))
    val fee = Longs.fromByteArray(bytes.slice(32, 40))
    val timestamp = Longs.fromByteArray(bytes.slice(40, 48))
    val signature = Signature25519(Signature @@ bytes.slice(48, 112))
    val inputLength = Ints.fromByteArray(bytes.slice(112, 116))
    val outputLength = Ints.fromByteArray(bytes.slice(116, 120))
    val s = 120
    val outElementLength = 32
    val useOutputs = (0 until inputLength).map { i =>
      ADKey @@ bytes.slice(s + (i * outElementLength), s + (i + 1) * outElementLength)
    }

    val s2 = s + (inputLength * outElementLength)
    val inElementLength = 45
    val createOutputs = (0 until outputLength).map { i =>
      (Address @@ Base58.encode(bytes.slice(s2 + i * inElementLength, s2 + (i + 1) * (inElementLength - 8))),
        Longs.fromByteArray(bytes.slice(s2 + (i + 1) * (inElementLength - 8), s2 + (i + 1) * inElementLength)))
    }

    PaymentTransaction(sender, fee, timestamp, signature, useOutputs, createOutputs)
  }
}
