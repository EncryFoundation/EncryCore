package encry.modifiers.state.box

import com.google.common.primitives.{Bytes, Longs, Shorts}
import encry.modifiers.state.box.EncryBox.BxTypeId
import encry.modifiers.state.box.proposition.{EncryProposition, PropositionSerializer}
import encry.settings.Algos
import encrywm.lang.backend.env.{ESObject, ESValue}
import encrywm.lib.Types
import io.circe.Encoder
import io.circe.syntax._
import scorex.core.serialization.Serializer

import scala.util.Try

/** Stores arbitrary data in EncryTL binary format. */
case class DataBox(override val proposition: EncryProposition,
                   override val nonce: Long,
                   data: Array[Byte])
  extends EncryBox[EncryProposition] {

  override type M = DataBox

  override val typeId: BxTypeId = DataBox.TypeId

  override def serializer: Serializer[M] = DataBoxSerializer

  override val esType: Types.ESProduct = Types.DataBox

  override def asVal: ESValue = ESValue(Types.DataBox.ident.toLowerCase, Types.DataBox)(convert)

  override def convert: ESObject = {
    val fields = Map(
      "proposition" -> ESValue("proposition", Types.ESProposition)(proposition.convert),
      "typeId" -> ESValue("typeId", Types.ESInt)(typeId.toInt),
      "id" -> ESValue("id", Types.ESByteVector)(id),
      "data" -> ESValue("data", Types.ESByteVector)(data)
    )
    ESObject(Types.DataBox.ident, fields, esType)
  }
}

object DataBox {

  val TypeId: BxTypeId = 4.toByte

  implicit val jsonEncoder: Encoder[DataBox] = (bx: DataBox) => Map(
    "type" -> TypeId.asJson,
    "id" -> Algos.encode(bx.id).asJson,
    "proposition" -> bx.proposition.asJson,
    "nonce" -> bx.nonce.asJson,
    "data" -> Algos.encode(bx.data).asJson,
  ).asJson
}

object DataBoxSerializer extends Serializer[DataBox] {

  override def toBytes(obj: DataBox): Array[Byte] = {
    val propBytes = PropositionSerializer.toBytes(obj.proposition)
    Bytes.concat(
      Shorts.toByteArray(propBytes.length.toShort),
      propBytes,
      Longs.toByteArray(obj.nonce),
      Shorts.toByteArray(obj.data.length.toShort),
      obj.data
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[DataBox] = Try {
    val propositionLen = Shorts.fromByteArray(bytes.take(2))
    val iBytes = bytes.drop(2)
    val proposition = PropositionSerializer.parseBytes(iBytes.take(propositionLen)).get
    val nonce = Longs.fromByteArray(iBytes.slice(propositionLen, propositionLen + 8))
    val dataLen = Shorts.fromByteArray(iBytes.slice(propositionLen + 8, propositionLen + 8 + 2))
    val data = iBytes.takeRight(dataLen)
    DataBox(proposition, nonce, data)
  }
}
