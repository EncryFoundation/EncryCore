package encry.modifiers.mempool

import com.google.common.primitives.{Bytes, Ints, Longs}
import encry.crypto.Address
import encry.modifiers.mempool.EncryTransaction.{TxTypeId, _}
import encry.modifiers.state.box.proposition.AddressProposition
import encry.modifiers.state.box.{EncryBaseBox, PaymentBox}
import encry.settings.Algos
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

case class CoinbaseTransaction(winnerProposition: PublicKey25519Proposition,
                               override val timestamp: Long,
                               var signature: Signature25519,
                               useOutputs: IndexedSeq[(ADKey, Amount)])
  extends EncryTransaction[AddressProposition] {

  override type M = CoinbaseTransaction

  override val typeId: TxTypeId = 0.toByte

  override val fee: Amount = 0L

  override val newBoxes: Traversable[EncryBaseBox] = Seq(
    PaymentBox(
      proposition = AddressProposition(Address @@ winnerProposition.address),
      nonce = nonceFromDigest(Algos.hash(txHash)),
      amount = useOutputs.map(_._2).sum
    )
  )

  override def json: Json = Map(
    "id" -> Base58.encode(id).asJson,
  ).asJson

  override def serializer: Serializer[M] = CoinbaseTransactionSerializer

  override lazy val txHash: Digest32 = Algos.hash(
    Bytes.concat(
      Array[Byte](typeId),
      winnerProposition.pubKeyBytes,
      scorex.core.utils.concatFixLengthBytes(useOutputs.map { case (key, amount) =>
        key ++ Longs.toByteArray(amount)
      }),
      Longs.toByteArray(timestamp),
    )
  )

  override lazy val messageToSign: Array[Byte] = txHash

  override lazy val semanticValidity: Try[Unit] = {
    // Signature validity checks.
    if (!signature.isValid(winnerProposition, messageToSign)) {
      log.info(s"<TX: $txHash> Invalid signature provided.")
      Failure(new Error("Invalid signature provided!"))
    }
    Success()
  }
}

object CoinbaseTransactionSerializer extends Serializer[CoinbaseTransaction] {

  override def toBytes(obj: CoinbaseTransaction): Array[Byte] = {
    Bytes.concat(
      obj.winnerProposition.pubKeyBytes,
      Longs.toByteArray(obj.timestamp),
      obj.signature.signature,
      Ints.toByteArray(obj.useOutputs.length),
      obj.useOutputs.foldLeft(Array[Byte]()) { case (arr, (key, amount)) =>
        arr ++ key ++ Longs.toByteArray(amount)
      }
    )
  }

  // TODO: Test.
  override def parseBytes(bytes: Array[Byte]): Try[CoinbaseTransaction] = Try{

    val sender = new PublicKey25519Proposition(PublicKey @@ bytes.slice(0,32))
    val timestamp = Longs.fromByteArray(bytes.slice(32,40))
    val signature = Signature25519(Signature @@ bytes.slice(40, 72))
    val inputLength = Ints.fromByteArray(bytes.slice(72, 76))
    val s = 76
    val outElementLength = 41
    val useOutputs = (0 until inputLength) map { i =>
      ADKey @@ bytes.slice(s + (i * outElementLength), s + (i + 1) * outElementLength - 8) ->
      Longs.fromByteArray(bytes.slice(s + (i * outElementLength) + 32, s + (i + 1) * outElementLength))
    }

    CoinbaseTransaction(sender, timestamp, signature, useOutputs)
  }
}
