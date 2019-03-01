package encry.modifiers.mempool.directive

import TransactionProto.TransactionProtoMessage.DirectiveProtoMessage
import TransactionProto.TransactionProtoMessage.DirectiveProtoMessage.DirectiveProto.{DataDirectiveProto, ScriptedAssetDirectiveProto}
import TransactionProto.TransactionProtoMessage.DirectiveProtoMessage.{ADKeyProto, DataDirectiveProtoMessage, DirectiveProto, ScriptedAssetDirectiveProtoMessage}
import com.google.common.primitives.{Bytes, Ints, Longs}
import com.google.protobuf.ByteString
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
import org.encryfoundation.common.utils.TaggedTypes.ADKey
import org.encryfoundation.prismlang.compiler.CompiledContract.ContractHash
import scorex.crypto.encode.Base16
import scorex.crypto.hash.Digest32

import scala.util.Try

case class ScriptedAssetDirective(contractHash: ContractHash,
                                  amount: Amount,
                                  tokenIdOpt: Option[ADKey] = None) extends Directive {

  override type M = ScriptedAssetDirective

  override val typeId: DTypeId = ScriptedAssetDirective.TypeId

  override def boxes(digest: Digest32, idx: Int): Seq[EncryBaseBox] =
    Seq(AssetBox(EncryProposition(contractHash), Utils.nonceFromDigest(digest ++ Ints.toByteArray(idx)), amount))

  override lazy val isValid: Boolean = amount > 0

  override def serializer: Serializer[M] = ScriptedAssetDirectiveSerializer

  override def toDbVersion(txId: ModifierId, numberInTx: Int): DirectiveDBVersion =
    DirectiveDBVersion(Base16.encode(txId), numberInTx, typeId, isValid, Base16.encode(contractHash), amount, "", tokenIdOpt.map(Base16.encode), "")

  override def toDirectiveProto: DirectiveProtoMessage = ScriptedAssetDirectiveProtoSerializer.toProto(this)
}

object ScriptedAssetDirective {

  val TypeId: DTypeId = 3.toByte

  implicit val jsonEncoder: Encoder[ScriptedAssetDirective] = (d: ScriptedAssetDirective) => Map(
    "typeId" -> d.typeId.asJson,
    "contractHash" -> Algos.encode(d.contractHash).asJson,
    "amount" -> d.amount.asJson,
    "tokenId" -> d.tokenIdOpt.map(id => Algos.encode(id)).asJson
  ).asJson

  implicit val jsonDecoder: Decoder[ScriptedAssetDirective] = (c: HCursor) => for {
    contractHash <- c.downField("contractHash").as[String]
    amount <- c.downField("amount").as[Long]
    tokenIdOpt <- c.downField("tokenId").as[Option[String]]
  } yield Algos.decode(contractHash)
    .map(ch => ScriptedAssetDirective(ch, amount, tokenIdOpt.flatMap(id => Algos.decode(id).map(ADKey @@ _).toOption)))
    .getOrElse(throw new Exception("Decoding failed"))
}

object ScriptedAssetDirectiveProtoSerializer extends ProtoDirectiveSerializer[ScriptedAssetDirective] {

  ///TODO REMOVE GET
  override def toProto(message: ScriptedAssetDirective): DirectiveProtoMessage =
    DirectiveProtoMessage().withScriptedAssetDirectiveProto(ScriptedAssetDirectiveProtoMessage()
      .withContractHash(ByteString.copyFrom(message.contractHash))
      .withAmount(message.amount)
      .withTokenIdOpt(message.tokenIdOpt.map(element => ADKeyProto().withTokenIdOpt(ByteString.copyFrom(element))).get))

  override def fromProto(message: DirectiveProtoMessage): Option[ScriptedAssetDirective] =
    message.directiveProto.scriptedAssetDirectiveProto match {
      case Some(value) => Some(ScriptedAssetDirective(
        value.contractHash.toByteArray,
        value.amount,
        value.tokenIdOpt.map(x => ADKey @@ x.tokenIdOpt.toByteArray))
      )
      case None => Option.empty[ScriptedAssetDirective]
    }
}

object ScriptedAssetDirectiveSerializer extends Serializer[ScriptedAssetDirective] {

  override def toBytes(obj: ScriptedAssetDirective): Array[Byte] =
    Bytes.concat(
      obj.contractHash,
      Longs.toByteArray(obj.amount),
      obj.tokenIdOpt.getOrElse(Array.empty)
    )

  override def parseBytes(bytes: Array[Byte]): Try[ScriptedAssetDirective] = Try {
    val contractHash: ContractHash = bytes.take(Constants.DigestLength)
    val amount: Amount = Longs.fromByteArray(bytes.slice(Constants.DigestLength, Constants.DigestLength + 8))
    val tokenIdOpt: Option[ADKey] = if ((bytes.length - (Constants.DigestLength + 8)) == Constants.ModifierIdSize) {
      Some(ADKey @@ bytes.takeRight(Constants.ModifierIdSize))
    } else None
    ScriptedAssetDirective(contractHash, amount, tokenIdOpt)
  }
}
