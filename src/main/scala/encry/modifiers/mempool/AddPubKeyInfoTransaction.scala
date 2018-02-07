package encry.modifiers.mempool

import com.google.common.primitives.{Bytes, Ints, Longs}
import encry.account.Address
import encry.modifiers.mempool.EncryTransaction.{TxTypeId, nonceFromDigest}
import encry.modifiers.state.box.proposition.{AddressProposition, HeightProposition}
import encry.modifiers.state.box.{AssetBox, EncryBaseBox, OpenBox, PubKeyInfoBox}
import encry.settings.Algos
import encry.view.history.Height
import io.circe.Json
import io.circe.syntax._
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.Box.Amount
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Digest32
import scorex.crypto.signatures.{PublicKey, Signature}

import scala.util.{Failure, Success, Try}

case class AddPubKeyInfoTransaction(override val proposition: PublicKey25519Proposition,
                                    override val fee: Amount,
                                    override val timestamp: Long,
                                    override val signature: Signature25519,
                                    override val useBoxes: IndexedSeq[ADKey],
                                    change: Amount,
                                    pubKeyBytes: PublicKey,
                                    pubKeyProofBytes: Signature,
                                    pubKeyInfo: String) extends EncryTransaction {

  override type M = AddPubKeyInfoTransaction

  override val length: Int = 216 + (32 * useBoxes.size) + pubKeyInfo.getBytes.length

  // Type of actual Tx type.
  override val typeId: TxTypeId = AddPubKeyInfoTransaction.typeId

  override val feeBox: Option[OpenBox] =
    Some(OpenBox(HeightProposition(Height @@ 0), nonceFromDigest(Algos.hash(txHash :+ OpenBox.typeId)), fee))

  private val pubKeyInfoBox =
    PubKeyInfoBox(
      AddressProposition(Address @@ proposition.address),
      nonceFromDigest(Algos.hash(txHash :+ PubKeyInfoBox.typeId)),
      pubKeyBytes
    )

  private val changeBox = if (change > 0) Some(AssetBox(
        AddressProposition(Address @@ proposition.address),
        nonceFromDigest(Algos.hash(txHash :+ OpenBox.typeId)),
        change)
      ) else None

  override val newBoxes: Traversable[EncryBaseBox] =
    changeBox.map(bx => Seq(feeBox.get, pubKeyInfoBox, bx)).getOrElse(Seq(feeBox.get, pubKeyInfoBox))

  override lazy val serializer: Serializer[M] = AddPubKeyInfoTransactionSerializer

  override def json: Json = Map(
    "id" -> Algos.encode(id).asJson,
    "proposition" -> Algos.encode(proposition.pubKeyBytes).asJson,
    "fee" -> fee.asJson,
    "timestamp" -> timestamp.asJson,
    "signature" -> Algos.encode(signature.signature).asJson,
    "inputs" -> useBoxes.map { id =>
      Map(
        "id" -> Algos.encode(id).asJson,
      ).asJson
    }.asJson,
    "publicKey" -> Algos.encode(pubKeyBytes).asJson,
    "publicKeyProof" -> Algos.encode(pubKeyProofBytes).asJson,
    "publicKeyInfo" -> pubKeyInfo.asJson
  ).asJson

  override lazy val txHash: Digest32 = AddPubKeyInfoTransaction
    .getHash(proposition, fee, timestamp, useBoxes, change, pubKeyBytes, pubKeyProofBytes, pubKeyInfo)

  override lazy val semanticValidity: Try[Unit] = {
    // Signature validity checks.
    if (!validSignature) {
      Failure(new Error("Invalid signature"))
    } else if (fee < minimalFee) {
      Failure(new Error("Fee amount too small"))
    } else
      Success()
  }

  // Message to sign for publicKey verification.
  lazy val pubKeyVerificationMessage: Array[Byte] = Algos.hash(proposition.bytes ++ signature.bytes)
}

object AddPubKeyInfoTransaction {

  val typeId: TxTypeId = 3.toByte

  // TODO: Use proposition.publicKeyBytes instead of PublicKeyProposition instance here.
  def getHash(proposition: PublicKey25519Proposition,
              fee: Amount,
              timestamp: Long,
              useBoxes: IndexedSeq[ADKey],
              change: Amount,
              pubKeyBytes: PublicKey,
              pubKeyProofBytes: Signature,
              pubKeyInfo: String): Digest32 = Algos.hash(
    Bytes.concat(
      Array[Byte](typeId),
      proposition.pubKeyBytes,
      useBoxes.foldLeft(Array[Byte]())(_ ++ _),
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee),
      pubKeyBytes,
      pubKeyProofBytes,
      pubKeyInfo.getBytes
    )
  )

  def getMessageToSign(proposition: PublicKey25519Proposition,
                       fee: Amount,
                       timestamp: Long,
                       useBoxes: IndexedSeq[ADKey],
                       change: Amount,
                       pubKeyBytes: PublicKey,
                       pubKeyProofBytes: Signature,
                       pubKeyInfo: String): Array[Byte] =
    getHash(proposition, fee, timestamp, useBoxes, change, pubKeyBytes, pubKeyProofBytes, pubKeyInfo)
}

object AddPubKeyInfoTransactionSerializer extends Serializer[AddPubKeyInfoTransaction] {

  override def toBytes(obj: AddPubKeyInfoTransaction): Array[Byte] = {
    Bytes.concat(
      obj.proposition.pubKeyBytes,
      Longs.toByteArray(obj.fee),
      Longs.toByteArray(obj.timestamp),
      obj.signature.signature,
      Ints.toByteArray(obj.useBoxes.length),
      obj.useBoxes.foldLeft(Array[Byte]())(_ ++ _),
      Longs.toByteArray(obj.change),
      obj.pubKeyBytes,
      obj.pubKeyProofBytes,
      obj.pubKeyInfo.getBytes
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[AddPubKeyInfoTransaction] = Try {

    val sender = new PublicKey25519Proposition(PublicKey @@ bytes.slice(0, 32))
    val fee = Longs.fromByteArray(bytes.slice(32, 40))
    val timestamp = Longs.fromByteArray(bytes.slice(40, 48))
    val signature = Signature25519(Signature @@ bytes.slice(48, 112))
    val inputLength = Ints.fromByteArray(bytes.slice(112, 116))
    val s = 116
    val outElementLength = 32
    val useOutputs = (0 until inputLength).map { i =>
      ADKey @@ bytes.slice(s + (i * outElementLength), s + (i + 1) * outElementLength)
    }
    val s2 = s + (outElementLength * inputLength)
    val change = Longs.fromByteArray(bytes.slice(s2, s2 + 8))
    val pubKeyDataStart = s2 + 8
    val pubKeyBytes = PublicKey @@ bytes.slice(pubKeyDataStart, pubKeyDataStart + 32)
    val pubKeyProofBytes = Signature @@ bytes.slice(pubKeyDataStart + 32, pubKeyDataStart + 32 + 64)
    val pubKeyInfo = new String(bytes.slice(pubKeyDataStart + 32 + 64, bytes.length), "UTF-8")
    AddPubKeyInfoTransaction(
      sender, fee, timestamp, signature, useOutputs, change, pubKeyBytes, pubKeyProofBytes, pubKeyInfo)
  }
}
