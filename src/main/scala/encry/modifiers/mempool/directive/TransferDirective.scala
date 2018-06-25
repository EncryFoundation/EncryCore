package encry.modifiers.mempool.directive

import com.google.common.primitives.{Bytes, Ints, Longs}
import encry.account
import encry.account.{Account, Address}
import encry.modifiers.Serializer
import encry.modifiers.mempool.directive.Directive.DTypeId
import encry.modifiers.state.box.{AssetBox, EncryBaseBox, EncryProposition}
import encry.settings.{Algos, Constants}
import encry.utils.Utils
import io.circe.{Decoder, Encoder, HCursor}
import io.circe.syntax._
import encry.modifiers.state.box.Box.Amount
import scorex.crypto.authds
import scorex.crypto.authds.ADKey
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Digest32
import supertagged.@@

import scala.util.Try

case class TransferDirective(address: Address,
                             amount: Amount,
                             tokenIdOpt: Option[ADKey] = None) extends Directive {

  override type M = TransferDirective

  override val typeId: DTypeId = TransferDirective.TypeId

  override def boxes(digest: Digest32, idx: Int): Seq[EncryBaseBox] =
    Seq(AssetBox(EncryProposition.accountLock(Account(address)),
      Utils.nonceFromDigest(digest ++ Ints.toByteArray(idx)), amount, tokenIdOpt))

  override lazy val isValid: Boolean = amount > 0 && Account.validAddress(address)

  override def serializer: Serializer[M] = TransferDirectiveSerializer

  lazy val isIntrinsic: Boolean = tokenIdOpt.isEmpty
}

object TransferDirective {

  val TypeId: DTypeId = 1.toByte

  implicit val jsonEncoder: Encoder[TransferDirective] = (d: TransferDirective) => Map(
    "typeId" -> d.typeId.asJson,
    "address" -> d.address.toString.asJson,
    "amount" -> d.amount.asJson,
    "tokenId" -> d.tokenIdOpt.map(id => Algos.encode(id)).getOrElse("null").asJson
  ).asJson

  implicit val jsonDecoder: Decoder[TransferDirective] = (c: HCursor) => {
    for {
      address <- c.downField("address").as[String]
      amount <- c.downField("amount").as[Long]
      tokenIdOpt <- c.downField("tokenId").as[Option[String]]
    } yield {
      TransferDirective(
        Address @@ address,
        amount,
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
      obj.tokenIdOpt.getOrElse(Array.empty)
    )

  override def parseBytes(bytes: Array[Byte]): Try[TransferDirective] = Try {
    val address: @@[String, account.Address.Tag] = Address @@ Base58.encode(bytes.take(Account.AddressLength))
    val amount: Amount = Longs.fromByteArray(bytes.slice(Account.AddressLength, Account.AddressLength + 8))
    val tokenIdOpt: Option[@@[Array[DTypeId], authds.ADKey.Tag]] = if ((bytes.length - (Account.AddressLength + 8)) == Constants.ModifierIdSize) {
      Some(ADKey @@ bytes.takeRight(Constants.ModifierIdSize))
    } else None
    TransferDirective(address, amount, tokenIdOpt)
  }
}
