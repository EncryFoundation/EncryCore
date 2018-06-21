package encry.modifiers.mempool.directive

import com.google.common.primitives.{Bytes, Ints, Shorts}
import encry.modifiers.Serializer
import encry.modifiers.mempool.directive.Directive.DTypeId
import encry.modifiers.state.box.proposition.EncryProposition
import encry.modifiers.state.box.{DataBox, EncryBaseBox}
import encry.settings.{Algos, Constants}
import encry.utils.Utils
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import org.encryfoundation.prismlang.compiler.{CompiledContract, CompiledContractSerializer}
import scorex.core.transaction.box.Box.Amount
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Digest32

import scala.util.Try

case class DataDirective(contract: CompiledContract, data: Array[Byte]) extends Directive {

  override type M = DataDirective

  override val typeId: DTypeId = DataDirective.TypeId

  override def boxes(digest: Digest32, idx: Int): Seq[EncryBaseBox] =
    Seq(DataBox(EncryProposition(contract), Utils.nonceFromDigest(digest ++ Ints.toByteArray(idx)), data))

  override val cost: Amount = 20

  override lazy val isValid: Boolean = data.length <= Constants.MaxDataLength && contract.bytes.lengthCompare(Short.MaxValue) <= 0

  override def serializer: Serializer[M] = DataDirectiveSerializer
}

object DataDirective {

  val TypeId: DTypeId = 5.toByte

  implicit val jsonEncoder: Encoder[DataDirective] = (d: DataDirective) => Map(
    "typeId" -> d.typeId.asJson,
    "contract" -> Base58.encode(d.contract.bytes).asJson,
    "data" -> Algos.encode(d.data).asJson
  ).asJson

  implicit val jsonDecoder: Decoder[DataDirective] = (c: HCursor) => {
    for {
      contractBytes <- c.downField("contract").as[String]
      dataEnc <- c.downField("data").as[String]
    } yield {
      val contract: CompiledContract = Algos.decode(contractBytes).flatMap(CompiledContractSerializer.parseBytes)
        .getOrElse(throw new Exception("Decoding failed"))
      val data: Array[Byte] = Algos.decode(dataEnc).getOrElse(throw new Exception("Data decoding failed"))
      DataDirective(contract, data)
    }
  }
}

object DataDirectiveSerializer extends Serializer[DataDirective] {

  override def toBytes(obj: DataDirective): Array[Byte] =
    Bytes.concat(
      Shorts.toByteArray(obj.contract.bytes.length.toShort),
      obj.contract.bytes,
      Shorts.toByteArray(obj.data.length.toShort),
      obj.data
    )

  override def parseBytes(bytes: Array[Byte]): Try[DataDirective] = {
    val scriptLen: Short = Shorts.fromByteArray(bytes.take(2))
    CompiledContractSerializer.parseBytes(bytes.slice(2, scriptLen)).map { contract =>
      val dataLen = Shorts.fromByteArray(bytes.slice(scriptLen + 2, scriptLen + 2 + 2))
      val data = bytes.slice(scriptLen + 2 + 2, scriptLen + 2 + 2 + dataLen)
      DataDirective(contract, data)
    }
  }
}
