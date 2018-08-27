package encry.modifiers.mempool.directive

import encry.ModifierId
import encry.modifiers.mempool.directive.Directive.DTypeId
import encry.modifiers.state.box.EncryBaseBox
import encry.utils.Logging
import io.circe._
import org.encryfoundation.common.serialization.BytesSerializable
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Digest32

trait  Directive extends BytesSerializable {

  val typeId: DTypeId
  val isValid: Boolean

  def boxes(digest: Digest32, idx: Int): Seq[EncryBaseBox]

  def toDbVersion(txId: ModifierId): DirectiveDBVersion
}

object Directive {

  type DTypeId = Byte

  implicit val jsonEncoder: Encoder[Directive] = {
    case td: TransferDirective => TransferDirective.jsonEncoder(td)
    case aid: AssetIssuingDirective => AssetIssuingDirective.jsonEncoder(aid)
    case sad: ScriptedAssetDirective => ScriptedAssetDirective.jsonEncoder(sad)
    case _ => throw new Exception("Incorrect directive type")
  }

  implicit val jsonDecoder: Decoder[Directive] = {
    Decoder.instance { c =>
      c.downField("typeId").as[DTypeId] match {
        case Right(s) => s match {
          case TransferDirective.TypeId => TransferDirective.jsonDecoder(c)
          case AssetIssuingDirective.TypeId => AssetIssuingDirective.jsonDecoder(c)
          case ScriptedAssetDirective.TypeId => ScriptedAssetDirective.jsonDecoder(c)
          case _ => Left(DecodingFailure("Incorrect directive typeID", c.history))
        }
        case Left(_) => Left(DecodingFailure("None typeId", c.history))
      }
    }
  }
}

case class DirectiveDBVersion(txId: String,
                              dTypeId: DTypeId,
                              isValid: Boolean,
                              contractHash: Array[Byte],
                              amount: Long,
                              address: String,
                              tokenIdOpt: Option[Array[Byte]],
                              data: Array[Byte]) extends Logging {
  def toDirective: Option[Directive] = {
    dTypeId match {
      case AssetIssuingDirective.TypeId => Some(AssetIssuingDirective(contractHash, amount))
      case TransferDirective.TypeId => Some(TransferDirective(address, amount, tokenIdOpt.map(ADKey @@ _)))
      case ScriptedAssetDirective.TypeId => Some(ScriptedAssetDirective(contractHash, amount, tokenIdOpt.map(ADKey @@ _)))
      case DataDirective.TypeId => Some(DataDirective(contractHash, data))
      case _ =>
        log.warn(s"Malformed directive from DB: $this")
        None
    }
  }
}