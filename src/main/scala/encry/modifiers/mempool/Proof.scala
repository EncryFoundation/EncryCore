package encry.modifiers.mempool

import encry.settings.Algos
import org.encryfoundation.prismlang.codec.PCodec
import org.encryfoundation.prismlang.core.wrapped.BoxedValue
import scorex.core.serialization.{BytesSerializable, Serializer}
import io.circe.{Decoder, Encoder, HCursor}
import io.circe.syntax._
import scodec.bits.BitVector

import scala.util.Try

case class Proof(value: BoxedValue, tagOpt: Option[String]) extends BytesSerializable {

  override type M = Proof

  override def serializer: Serializer[Proof] = ProofSerializer
}

object Proof {

  implicit val jsonEncoder: Encoder[Proof] = (p: Proof) => Map(
    "serializedValue" -> Algos.encode(PCodec.boxedValCodec.encode(p.value).require.toByteArray).asJson,
    "tag" -> p.tagOpt.map(_.asJson)
  ).asJson

  implicit val jsonDecoder: Decoder[Proof] = (c: HCursor) => {
    for {
      serializedValue <- c.downField("serializedValue").as[String]
      tag <- c.downField("tag").as[Option[String]]
    } yield {
      Algos.decode(serializedValue)
        .map(bytes => PCodec.boxedValCodec.decode(BitVector(bytes)).require.value)
        .map(value => Proof(value, tag)).getOrElse(throw new Exception("Decoding failed"))
    }
  }
}

object ProofSerializer extends Serializer[Proof] {

  override def toBytes(obj: Proof): Array[Byte] = ???

  override def parseBytes(bytes: Array[Byte]): Try[Proof] = ???
}
