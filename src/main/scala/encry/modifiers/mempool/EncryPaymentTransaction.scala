package encry.modifiers.mempool

import encry.modifiers.mempool.EncryTransaction._
import encry.modifiers.state.box.body.PaymentBoxBody
import encry.modifiers.state.box.EncryPaymentBox
import encry.modifiers.state.box.unlockers.EncryPaymentBoxUnlocker
import encry.settings.Algos
import encry.crypto.Address
import com.google.common.primitives.{Bytes, Ints, Longs}
import encry.modifiers.state.box.proposition.AddressProposition
import io.circe.Json
import io.circe.syntax._
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.box.Box.Amount
import scorex.crypto.authds.ADKey
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Digest32
import scorex.core.transaction.proof.Signature25519
import scorex.crypto.signatures.{PublicKey, Signature}

import scala.util.{Failure, Success, Try}

case class EncryPaymentTransaction(senderProposition: PublicKey25519Proposition,
                                   override val fee: Amount,
                                   override val timestamp: Long,
                                   var signature: Signature25519,
                                   useOutputs: IndexedSeq[ADKey],
                                   createOutputs: IndexedSeq[(Address, Amount)])
  extends EncryTransaction[PublicKey25519Proposition, AddressProposition, PaymentBoxBody] {

  override type M = EncryPaymentTransaction

  // Type of actual Tx type.
  override val typeId: TxTypeId = 1.toByte

  override val unlockers: Traversable[EncryPaymentBoxUnlocker] = useOutputs.map{
    boxId => EncryPaymentBoxUnlocker(boxId, signature)
  }

  override val newBoxes: Traversable[EncryPaymentBox] = createOutputs.zipWithIndex.map { case ((addr, amount), idx) =>
    val nonce = nonceFromDigest(Algos.hash(txHash ++ Ints.toByteArray(idx)))
    EncryPaymentBox(AddressProposition(addr), nonce, PaymentBoxBody(amount))
  }

  override def serializer: Serializer[EncryPaymentTransaction] = EncryPaymentTransactionSerializer

  override def json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "inputs" -> useOutputs.map { id =>
      Map(
        "id" -> Algos.encode(id).asJson,
        "signature" -> Base58.encode(signature.bytes).asJson
      ).asJson
    }.asJson,
    "outputs" -> createOutputs.map { case (_, amount) =>
      Map(
        "script" -> "".asJson,
        "amount" -> amount.asJson
      ).asJson
    }.asJson
  ).asJson

  // Which fields should be included into tx hash?
  override lazy val txHash: Digest32 = Algos.hash(
    Bytes.concat(
      scorex.core.utils.concatFixLengthBytes(useOutputs),
      scorex.core.utils.concatFixLengthBytes(createOutputs.map { case (addr, amount) =>
        AddressProposition.addrBytes(addr) ++ Longs.toByteArray(amount)
      }),
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee)
    )
  )

  override lazy val messageToSign: Array[Byte] = txHash

  override lazy val semanticValidity: Try[Unit] = {
    // Signature validity checks.
    if (!signature.isValid(senderProposition, messageToSign)) {
      log.info(s"<TX: $txHash> Invalid signature provided.")
      Failure(new Error("Invalid signature provided!"))
    }
    // `Amount` & `Address` validity checks.
    if (!createOutputs.forall { i =>
      i._2 > 0 && AddressProposition.validAddress(i._1)
    }) {
      log.info(s"<TX: $txHash> Invalid content.")
      Failure(new Error("Transaction invalid!"))
    }
    Success()
  }

}

object EncryPaymentTransactionSerializer extends Serializer[EncryPaymentTransaction] {

  override def toBytes(obj: EncryPaymentTransaction): Array[Byte] = {
    Bytes.concat(
      obj.senderProposition.pubKeyBytes,
      Longs.toByteArray(obj.fee),
      Longs.toByteArray(obj.timestamp),
      obj.signature.signature,
      Ints.toByteArray(obj.useOutputs.length),
      Ints.toByteArray(obj.createOutputs.length),
      obj.useOutputs.foldLeft(Array[Byte]())((a, b) => Bytes.concat(a, b)),
      obj.createOutputs.foldLeft(Array[Byte]())((a, b) => Bytes.concat(a, b._1.getBytes, Longs.toByteArray(b._2)))
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[EncryPaymentTransaction] = Try{

    val sender = new PublicKey25519Proposition(PublicKey @@ bytes.slice(0,32))
    val fee = Longs.fromByteArray(bytes.slice(32,40))
    val timestamp = Longs.fromByteArray(bytes.slice(40,48))
    val signature = Signature25519(Signature @@ bytes.slice(48,80))
    val inputLength = Ints.fromByteArray(bytes.slice(80,84))
    val outputLength = Ints.fromByteArray(bytes.slice(84,88))
    val s = 88
    val outElementLength = 32
    val useOutputs = (0 until inputLength) map { i =>
      ADKey @@ bytes.slice(s + (i * outElementLength), s + (i + 1) * outElementLength)
    }

    val s2 = s + (inputLength * outElementLength)
    val inElementLength = 40
    val createOutputs = (0 until outputLength) map { i =>
      // Longs.fromByteArray(bytes.slice(s2 + i * elementLength, s2 + (i + 1) * elementLength))
      (Address @@ bytes.slice(s2 + i * inElementLength, s2 + (i + 1) * (inElementLength-8)).toString,
        Longs.fromByteArray(bytes.slice(s2 + i * (inElementLength-8), s2 + (i + 1) * inElementLength)))
    }

    EncryPaymentTransaction(sender, fee, timestamp, signature, useOutputs, createOutputs)
  }
}