package encry.modifiers.mempool.directive

import com.google.common.primitives.{Bytes, Ints, Longs}
import encry.account.{Account, Address}
import encry.modifiers.mempool.directive.Directive.DTypeId
import encry.modifiers.state.box.{AssetBox, EncryBaseBox}
import encry.settings.{Algos, Constants}
import encry.utils.Utils
import io.circe.{Decoder, Encoder, HCursor}
import io.circe.syntax._
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.Box.Amount
import scorex.crypto.authds.ADKey
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Digest32

import scala.util.Try

case class TransferDirective(address: Address,
                             amount: Amount,
                             override val idx: Int,
                             tokenIdOpt: Option[ADKey] = None) extends Directive {

  override type M = TransferDirective

  override val typeId: DTypeId = TransferDirective.TypeId

  override def boxes(digest: Digest32): Seq[EncryBaseBox] =
      Seq(AssetBox(address, Utils.nonceFromDigest(digest ++ Ints.toByteArray(idx)), amount, tokenIdOpt))

  override val cost: Amount = 4

  override lazy val isValid: Boolean = amount > 0 && Account.validAddress(address)

  override def serializer: Serializer[M] = TransferDirectiveSerializer

  lazy val isIntrinsic: Boolean = tokenIdOpt.isEmpty
}

object TransferDirective {

  val TypeId: DTypeId = 1.toByte

  implicit val jsonEncoder: Encoder[TransferDirective] = (d: TransferDirective) => Map(
    "typeId" -> d.typeId.asJson,
    "verboseType" -> "TRANSFER".asJson,
    "address" -> d.address.toString.asJson,
    "amount" -> d.amount.asJson,
    "tokenId" -> d.tokenIdOpt.map(id => Algos.encode(id)).getOrElse("null").asJson,
    "idx" -> d.idx.asJson
  ).asJson

  implicit val jsonDecoder: Decoder[TransferDirective] = (c: HCursor) => {
    for {
      address <- c.downField("address").as[String]
      amount <- c.downField("amount").as[Long]
      idx <- c.downField("idx").as[Int]
      tokenIdOpt <- c.downField("tokenId").as[Option[String]]
    } yield {
      TransferDirective(
        Address @@ address,
        amount,
        idx,
        tokenIdOpt.flatMap(id => Algos.decode(id).map(ADKey @@ _).toOption)
      )
    }
  }
}

object TransferDirectiveSerializer extends Serializer[TransferDirective] {

  override def toBytes(obj: TransferDirective): Array[Byte] =
    Bytes.concat(
      Account.decodeAddress(obj.address),
      Longs.toByteArray(obj.amount),
      Ints.toByteArray(obj.idx),
      obj.tokenIdOpt.getOrElse(Array.empty)
    )

  override def parseBytes(bytes: Array[Byte]): Try[TransferDirective] = Try {
    val address = Address @@ Base58.encode(bytes.take(Account.AddressLength))
    val amount = Longs.fromByteArray(bytes.slice(Account.AddressLength, Account.AddressLength + 8))
    val idx = Ints.fromByteArray(bytes.slice(Account.AddressLength + 8, Account.AddressLength + 8 + 4))
    val tokenIdOpt = if ((bytes.length - (Account.AddressLength + 8 + 4)) == Constants.ModifierIdSize) {
      Some(ADKey @@ bytes.takeRight(Constants.ModifierIdSize))
    } else None
    TransferDirective(address, amount, idx, tokenIdOpt)
  }
}
