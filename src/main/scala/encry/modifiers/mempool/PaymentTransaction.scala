package encry.modifiers.mempool

import com.google.common.primitives.{Bytes, Ints, Longs}
import encry.crypto.Address
import encry.modifiers.mempool.EncryTransaction._
import encry.modifiers.state.box.proposition.AddressProposition
import encry.modifiers.state.box.unlockers.AssetBoxUnlocker
import encry.modifiers.state.box.{EncryBaseBox, OpenBox, AssetBox}
import encry.settings.{Algos, Constants}
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

import scala.util.{Failure, Success, Try}

case class PaymentTransaction(override val proposition: PublicKey25519Proposition,
                              override val fee: Amount,
                              override val timestamp: Long,
                              var signature: Signature25519,
                              useBoxes: IndexedSeq[ADKey],
                              createBoxes: IndexedSeq[(Address, Amount)])
  extends EncryTransaction[PublicKey25519Proposition] {

  override type M = PaymentTransaction

  override val length: Int = 80 + (33 * useBoxes.size) + (28 * createBoxes.size)

  // Type of actual Tx type.
  override val typeId: TxTypeId = PaymentTransaction.typeId

  override val unlockers: Traversable[AssetBoxUnlocker] =
    useBoxes.map(boxId => AssetBoxUnlocker(boxId, signature))

  override val newBoxes: Traversable[EncryBaseBox] =
    Seq(OpenBox(nonceFromDigest(Algos.hash(txHash)), fee)) ++
      createBoxes.zipWithIndex.map { case ((addr, amount), idx) =>
        val nonce = nonceFromDigest(Algos.hash(txHash ++ Ints.toByteArray(idx)))
        AssetBox(AddressProposition(addr), nonce, amount)
      }

  override def serializer: Serializer[M] = PaymentTransactionSerializer

  override def json: Json = Map(
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

  override lazy val txHash: Digest32 = PaymentTransaction.getHash(proposition, fee, timestamp, useBoxes, createBoxes)

  override lazy val messageToSign: Array[Byte] = txHash

  override lazy val semanticValidity: Try[Unit] = {
    // Signature validity checks.
    if (!signature.isValid(proposition, messageToSign)) {
      log.info(s"<TX: $txHash> Invalid signature provided.")
      Failure(new Error("Invalid signature provided!"))
    }
    // `Amount` & `Address` validity checks.
    if (!createBoxes.forall { i =>
      i._2 > 0 && AddressProposition.validAddress(i._1)
    }) {
      log.info(s"<TX: $txHash> Invalid content.")
      Failure(new Error("Transaction invalid!"))
    }
    // `Fee` amount check.
    if (fee < (Constants.feeMinAmount + Constants.txByteCost * serializer.toBytes(this).length))
      Failure(new Error("Fee amount too small."))

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
        AddressProposition.addrBytes(addr) ++ Longs.toByteArray(amount)
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
      obj.createBoxes.foldLeft(Array[Byte]())((a, b) => Bytes.concat(a, b._1.getBytes, Longs.toByteArray(b._2)))
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[PaymentTransaction] = Try{

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

    PaymentTransaction(sender, fee, timestamp, signature, useOutputs, createOutputs)
  }
}