package encry.modifiers.mempool

import com.google.common.primitives.{Bytes, Ints, Longs}
import encry.crypto.PublicKey25519
import encry.modifiers.mempool.directive.{CoinbaseDirective, Directive, DirectiveSerializer}
import encry.modifiers.state.box.AssetBox
import encry.modifiers.state.box.proof.Signature25519
import encry.modifiers.state.box.proposition.OpenProposition
import encry.settings.{Algos, Constants}
import encry.utils.Utils
import encrywm.backend.env.{ESObject, ESValue}
import encrywm.lib.Types
import encrywm.lib.Types.{ESByteVector, ESList, ESLong, ESTransaction}
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
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
                            override val directives: IndexedSeq[Directive]) extends EncryBaseTransaction {

  override type M = EncryTransaction

  override lazy val length: Int = this.bytes.length

  override val maxSize: Int = EncryTransaction.MaxSize

  override val feeBox: Option[AssetBox] =
    if (fee > 0) Some(AssetBox(OpenProposition, Utils.nonceFromDigest(Algos.hash(txHash)), fee))
    else None

  override lazy val serializer: Serializer[M] = EncryTransactionSerializer

  override lazy val txHash: Digest32 =
    EncryTransaction.getHash(accountPubKey, fee, timestamp, unlockers, directives)

  override lazy val isCoinbase: Boolean = directives.head.isInstanceOf[CoinbaseDirective]

  override lazy val semanticValidity: Try[Unit] = {

    def validOrder(s: Seq[Int]): Boolean =
      Try(s.foldLeft(-1)((a, b) => if (b > a) b else throw new Error("Invalid order"))).isSuccess

    if (!validSize) {
      Failure(new Error("Invalid size"))
    } else if (fee < 0) {
      Failure(new Error("Negative fee"))
    } else if (!validSignature) {
      Failure(new Error("Invalid signature"))
    } else if (!directives.forall(_.isValid)) {
      Failure(new Error("Bad outputs"))
    } else if (!validOrder(directives.map(_.idx))) {
      Failure(new Error("Illegal directives order"))
    } else Success()
  }

  override val esType: Types.ESProduct = ESTransaction

  override def asVal: ESValue = ESValue(ESTransaction.ident.toLowerCase, ESTransaction)(convert)

  override def convert: ESObject = {
    val fields = Map(
      "accountPubKey" -> ESValue("accountPubKey", ESByteVector)(accountPubKey.pubKeyBytes),
      "fee" -> ESValue("fee", ESLong)(fee),
      "signature" -> ESValue("signature", ESByteVector)(signature.signature),
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
    "accountPubKey" -> Algos.encode(tx.accountPubKey.pubKeyBytes).asJson,
    "fee" -> tx.fee.asJson,
    "timestamp" -> tx.timestamp.asJson,
    "signature" -> Algos.encode(tx.signature.signature).asJson,
    "unlockers" -> tx.unlockers.map(_.asJson).asJson,
    "directives" -> tx.directives.map(_.asJson).asJson
  ).asJson

  implicit val jsonDecoder: Decoder[EncryTransaction] = (c: HCursor) => {
    for {
      accountPubKey <- c.downField("accountPubKey").as[String]
      fee <- c.downField("fee").as[Long]
      timestamp <- c.downField("timestamp").as[Long]
      signature <- c.downField("signature").as[String]
      unlockers <- c.downField("unlockers").as[IndexedSeq[Unlocker]]
      directives <- c.downField("directives").as[IndexedSeq[Directive]]
    } yield {
      EncryTransaction(
        PublicKey25519(PublicKey @@ Algos.decode(accountPubKey).get),
        fee,
        timestamp,
        Signature25519(Signature @@ Algos.decode(signature).get),
        unlockers,
        directives
      )
    }
  }

  def getHash(accountPubKey: PublicKey25519,
              fee: Amount,
              timestamp: Long,
              unlockers: IndexedSeq[Unlocker],
              directives: IndexedSeq[Directive]): Digest32 = Algos.hash(
    Bytes.concat(
      accountPubKey.pubKeyBytes,
      unlockers.map(_.bytesWithoutProof).foldLeft(Array[Byte]())(_ ++ _),
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
      obj.unlockers.map(u => Ints.toByteArray(u.bytes.length) ++ u.bytes).foldLeft(Array[Byte]())(_ ++ _),
      obj.directives.map { d =>
        val bytes = DirectiveSerializer.toBytes(d)
        Ints.toByteArray(bytes.length) ++ bytes
      }.reduceLeft(_ ++ _)
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
      UnlockerSerializer.parseBytes(leftBytes1.slice(shift + 4, shift + 4 + len)).map(u => (acc :+ u, shift + 4 + len))
        .getOrElse(throw new Exception("Serialization failed."))
    }
    val leftBytes2 = leftBytes1.drop(unlockersLen)
    val directives = (0 until directivesQty).foldLeft(IndexedSeq[Directive](), 0) { case ((acc, shift), _) =>
      val len = Ints.fromByteArray(leftBytes2.slice(shift, shift + 4))
      DirectiveSerializer.parseBytes(leftBytes2.slice(shift + 4, shift + 4 + len)).map(d => (acc :+ d, shift + 4 + len))
        .getOrElse(throw new Exception("Serialization failed."))
    }._1

    EncryTransaction(accPubKey, fee, timestamp, signature, unlockers, directives)
  }
}
