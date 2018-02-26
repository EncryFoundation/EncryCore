package encry.modifiers.mempool

import com.google.common.primitives.{Bytes, Ints, Longs}
import encry.crypto.{PublicKey25519, Signature25519}
import encry.modifiers.mempool.EncryTransaction.TxTypeId
import encry.modifiers.state.box.proposition.AccountProposition
import encry.modifiers.state.box.{AssetBox, EncryBaseBox, OpenBox}
import encry.settings.Algos
import encry.utils.Utils
import encry.view.history.Height
import encry.view.state.UtxoState
import io.circe.Json
import io.circe.syntax._
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.Box.Amount
import scorex.crypto.authds.ADKey
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Digest32
import scorex.crypto.signatures.{PublicKey, Signature}

import scala.util.{Failure, Success, Try}

case class CoinbaseTransaction(override val accountPubKey: PublicKey25519,
                               override val timestamp: Long,
                               override val signature: Signature25519,
                               override val useBoxes: IndexedSeq[ADKey],
                               amount: Amount,
                               height: Height) extends EncryTransaction {

  override type M = CoinbaseTransaction

  override val length: Int = 72 + (41 * useBoxes.size)

  override val maxSize: Int = CoinbaseTransaction.maxSize

  override val typeId: TxTypeId = CoinbaseTransaction.typeId

  // Zero for coinbase.
  override val fee: Amount = 0L

  override val feeBox: Option[OpenBox] = None

  private val commissionBox = if (amount > 0) Some(AssetBox(
    proposition = AccountProposition(accountPubKey.address),
    nonce = Utils.nonceFromDigest(Algos.hash(txHash)),
    amount = amount)) else None

  override val newBoxes: Seq[EncryBaseBox] = {
    val openBox = UtxoState.newOpenBoxAt(height, seed = timestamp * length)
    commissionBox.map(cbx => Seq(openBox, cbx)).getOrElse(Seq(openBox))
  }

  override def json: Json = Map(
    "type" -> "Coinbase".asJson,
    "id" -> Base58.encode(id).asJson,
    "inputs" -> useBoxes.map { id =>
      Map(
        "id" -> Algos.encode(id).asJson,
        "signature" -> Base58.encode(signature.bytes).asJson
      ).asJson
    }.asJson,
    "outputs" -> newBoxes.map { case bx: AssetBox =>
      Map(
        "script" -> "".asJson,
        "amount" -> bx.amount.asJson
      ).asJson
    }.asJson
  ).asJson

  override lazy val serializer: Serializer[M] = CoinbaseTransactionSerializer

  override lazy val txHash: Digest32 = CoinbaseTransaction.getHash(accountPubKey, useBoxes, timestamp, amount, height)

  override lazy val semanticValidity: Try[Unit] = {
    if (!validSize) {
      Failure(new Error("Invalid size"))
    } else if (!validSignature) {
      Failure(new Error("Invalid signature"))
    } else Success()
  }
}

object CoinbaseTransaction {

  val maxSize: Int = 300

  val typeId: TxTypeId = 0.toByte

  def getHash(accountPubKey: PublicKey25519,
              useBoxes: IndexedSeq[ADKey],
              timestamp: Long,
              amount: Amount,
              height: Height): Digest32 = Algos.hash(
    Bytes.concat(
      Array[Byte](typeId),
      accountPubKey.pubKeyBytes,
      Longs.toByteArray(timestamp),
      useBoxes.foldLeft(Array[Byte]()) { case (arr, key) =>
        arr ++ key
      },
      Longs.toByteArray(amount),
      Ints.toByteArray(height)
    )
  )

  def getMessageToSign(accountPubKey: PublicKey25519,
                       useBoxes: IndexedSeq[ADKey],
                       timestamp: Long,
                       amount: Amount,
                       height: Height): Array[Byte] = getHash(accountPubKey, useBoxes, timestamp, amount, height)
}

object CoinbaseTransactionSerializer extends Serializer[CoinbaseTransaction] {

  override def toBytes(obj: CoinbaseTransaction): Array[Byte] = {
    Bytes.concat(
      obj.accountPubKey.pubKeyBytes,
      Longs.toByteArray(obj.timestamp),
      obj.signature.signature,
      Longs.toByteArray(obj.amount),
      Ints.toByteArray(obj.height),
      Ints.toByteArray(obj.useBoxes.length),
      obj.useBoxes.foldLeft(Array[Byte]()) { case (arr, key) =>
        arr ++ key
      }
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[CoinbaseTransaction] = Try {

    val sender = PublicKey25519(PublicKey @@ bytes.slice(0, 32))
    val timestamp = Longs.fromByteArray(bytes.slice(32, 40))
    val signature = Signature25519(Signature @@ bytes.slice(40, 104))
    val amount = Longs.fromByteArray(bytes.slice(104, 112))
    val height = Height @@ Ints.fromByteArray(bytes.slice(112, 116))
    val inputLength = Ints.fromByteArray(bytes.slice(116, 120))
    val s = 120
    val outElementLength = 32
    val useBoxes = (0 until inputLength) map { i =>
      ADKey @@ bytes.slice(s + (i * outElementLength), s + (i + 1) * outElementLength)
    }

    CoinbaseTransaction(sender, timestamp, signature, useBoxes, amount, height)
  }
}
