package encry.modifiers.state.box

import com.google.common.primitives.{Bytes, Longs, Shorts}
import encry.modifiers.serialization.Serializer
import encry.modifiers.state.box.EncryBox.BxTypeId
import encry.settings.Algos
import io.circe.Encoder
import io.circe.syntax._
import org.encryfoundation.prismlang.core.Types
import org.encryfoundation.prismlang.core.wrapped.{PObject, PValue}

import scala.util.Try

/** Stores arbitrary data in EncryTL binary format. */
case class DataBox(override val proposition: EncryProposition,
                   override val nonce: Long,
                   data: Array[Byte])
  extends EncryBox[EncryProposition] {

  override type M = DataBox

  override val typeId: BxTypeId = DataBox.TypeId

  override def serializer: Serializer[M] = DataBoxSerializer

  override val tpe: Types.Product = Types.DataBox

  override def asVal: PValue = PValue(convert, Types.DataBox)

  override def convert: PObject = {
    val fields = Map(
      "contractHash" -> PValue(proposition.contractHash, Types.PCollection.ofByte),
      "typeId" -> PValue(typeId, Types.PInt),
      "id" -> PValue(id, Types.PInt),
      "data" -> PValue(data, Types.PCollection.ofByte)
    )
    PObject(fields, tpe)
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
    val propBytes: Array[BxTypeId] = EncryPropositionSerializer.toBytes(obj.proposition)
    Bytes.concat(
      Shorts.toByteArray(propBytes.length.toShort),
      propBytes,
      Longs.toByteArray(obj.nonce),
      Shorts.toByteArray(obj.data.length.toShort),
      obj.data
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[DataBox] = Try {
    val propositionLen: Short = Shorts.fromByteArray(bytes.take(2))
    val iBytes: Array[BxTypeId] = bytes.drop(2)
    val proposition: EncryProposition = EncryPropositionSerializer.parseBytes(iBytes.take(propositionLen)).get
    val nonce: Long = Longs.fromByteArray(iBytes.slice(propositionLen, propositionLen + 8))
    val dataLen: Short = Shorts.fromByteArray(iBytes.slice(propositionLen + 8, propositionLen + 8 + 2))
    val data: Array[BxTypeId] = iBytes.takeRight(dataLen)
    DataBox(proposition, nonce, data)
  }
}
