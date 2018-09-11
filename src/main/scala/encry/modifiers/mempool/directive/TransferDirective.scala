package encry.modifiers.mempool.directive

import com.google.common.primitives.{Bytes, Ints, Longs}
import encry.utils.CoreTaggedTypes.ModifierId
import encry.modifiers.mempool.directive.Directive.DTypeId
import encry.modifiers.state.box.Box.Amount
import encry.modifiers.state.box.{AssetBox, EncryBaseBox, EncryProposition}
import encry.settings.Constants
import encry.utils.Utils
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import org.encryfoundation.common.Algos
import org.encryfoundation.common.serialization.Serializer
import org.encryfoundation.common.transaction.EncryAddress
import org.encryfoundation.common.transaction.EncryAddress.Address
import scorex.crypto.encode.Base16
import org.encryfoundation.common.utils.TaggedTypes.ADKey
import scorex.crypto.hash.Digest32
import supertagged.@@
import scala.util.Try

case class TransferDirective(address: Address,
                             amount: Amount,
                             tokenIdOpt: Option[ADKey] = None) extends Directive {

  override type M = TransferDirective

  override val typeId: DTypeId = TransferDirective.TypeId

  override def boxes(digest: Digest32, idx: Int): Seq[EncryBaseBox] =
    Seq(AssetBox(EncryProposition.addressLocked(address),
      Utils.nonceFromDigest(digest ++ Ints.toByteArray(idx)), amount, tokenIdOpt))

  override lazy val isValid: Boolean = amount > 0 && EncryAddress.resolveAddress(address).isSuccess

  override def serializer: Serializer[M] = TransferDirectiveSerializer

  override def toDbVersion(txId: ModifierId): DirectiveDBVersion =
    DirectiveDBVersion(Base16.encode(txId), typeId, isValid, "", amount, address, tokenIdOpt.map(Base16.encode), "")
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
    } yield TransferDirective(
      address,
      amount,
      tokenIdOpt.flatMap(id => Algos.decode(id).map(ADKey @@ _).toOption)
    )

  }
}

object TransferDirectiveSerializer extends Serializer[TransferDirective] {

  override def toBytes(obj: TransferDirective): Array[Byte] = {
    val address: Array[Byte] = obj.address.getBytes(Algos.charset)
    address.length.toByte +: Bytes.concat(
      address,
      Longs.toByteArray(obj.amount),
      obj.tokenIdOpt.getOrElse(Array.empty)
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[TransferDirective] = Try {
    val addressLen: Int = bytes.head.toInt
    val address: Address = new String(bytes.slice(1, 1 + addressLen), Algos.charset)
    val amount: Amount = Longs.fromByteArray(bytes.slice(1 + addressLen, 1 + addressLen + 8))
    val tokenIdOpt: Option[@@[Array[DTypeId], ADKey.Tag]] = if ((bytes.length - (1 + addressLen + 8)) == Constants.ModifierIdSize) {
      Some(ADKey @@ bytes.takeRight(Constants.ModifierIdSize))
    } else None
    TransferDirective(address, amount, tokenIdOpt)
  }
}
