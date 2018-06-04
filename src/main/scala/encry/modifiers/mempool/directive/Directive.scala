package encry.modifiers.mempool.directive

import encry.modifiers.mempool.directive.Directive.DTypeId
import encry.modifiers.state.box.EncryBaseBox
import io.circe._
import scorex.core.serialization.BytesSerializable
import scorex.core.transaction.box.Box.Amount
import scorex.crypto.hash.Digest32

// Directive is sub-modifier of the state.
trait Directive extends BytesSerializable {

  val typeId: DTypeId

  val idx: Int

  val cost: Amount

  val isValid: Boolean

  def boxes(digest: Digest32): Seq[EncryBaseBox]
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

