package encry.modifiers.state.box

import BoxesProto.BoxProtoMessage
import com.google.common.primitives.Longs
import encry.modifiers.state.box.EncryBox.BxTypeId
import io.circe.{Decoder, DecodingFailure, Encoder}
import org.encryfoundation.common.Algos
import org.encryfoundation.common.utils.TaggedTypes.ADKey
import org.encryfoundation.prismlang.core.wrapped.{PObject, PValue}
import org.encryfoundation.prismlang.core.{PConvertible, Types}

import scala.util.Try

trait EncryBaseBox extends Box[EncryProposition] with PConvertible {

  val typeId: BxTypeId

  val nonce: Long

  def serializeToProto: BoxProtoMessage

  override lazy val id: ADKey = ADKey @@ Algos.hash(Longs.toByteArray(nonce)).updated(0, typeId)

  def isAmountCarrying: Boolean = this.isInstanceOf[MonetaryBox]

  override val tpe: Types.Product = Types.EncryBox

  override def asVal: PValue = PValue(asPrism, tpe)

  lazy val baseFields: Map[String, PValue] = Map(
    "contractHash" -> PValue(proposition.contractHash, Types.PCollection.ofByte),
    "typeId"       -> PValue(typeId.toLong, Types.PInt),
    "id"           -> PValue(id, Types.PCollection.ofByte)
  )

  def asPrism: PObject = PObject(baseFields, tpe)
}

trait BaseBoxProtoSerialize[T] {

  def toProto(t: T): BoxProtoMessage

  def fromProto(b: Array[Byte]): Try[T]
}

object EncryBaseBox {

  implicit val jsonEncoder: Encoder[EncryBaseBox] = {
    case ab: AssetBox         => AssetBox.jsonEncoder(ab)
    case db: DataBox          => DataBox.jsonEncoder(db)
    case aib: TokenIssuingBox => TokenIssuingBox.jsonEncoder(aib)
  }

  implicit val jsonDecoder: Decoder[EncryBaseBox] = {
    Decoder.instance { c =>
      c.downField("type").as[BxTypeId] match {
        case Right(s) => s match {
          case AssetBox.TypeId        => AssetBox.jsonDecoder(c)
          case DataBox.TypeId         => DataBox.jsonDecoder(c)
          case TokenIssuingBox.TypeId => TokenIssuingBox.jsonDecoder(c)
          case _ => Left(DecodingFailure("Incorrect directive typeID", c.history))
        }
        case Left(_) => Left(DecodingFailure("None typeId", c.history))
      }
    }
  }
}