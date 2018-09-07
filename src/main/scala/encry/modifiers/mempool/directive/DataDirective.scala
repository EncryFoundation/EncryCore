package encry.modifiers.mempool.directive

import com.google.common.primitives.{Bytes, Ints}
import encry.modifiers.mempool.directive.Directive.DTypeId
import encry.modifiers.state.box.{DataBox, EncryBaseBox, EncryProposition}
import encry.settings.Constants
import encry.utils.CoreTaggedTypes.ModifierId
import encry.utils.Utils
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import org.encryfoundation.common.Algos
import org.encryfoundation.common.serialization.Serializer
import org.encryfoundation.prismlang.compiler.CompiledContract.ContractHash
import scorex.crypto.encode.Base16
import scorex.crypto.hash.Digest32
import scala.util.Try

case class DataDirective(contractHash: ContractHash, data: Array[Byte]) extends Directive {

  override type M = DataDirective

  override val typeId: DTypeId = DataDirective.TypeId

  override def boxes(digest: Digest32, idx: Int): Seq[EncryBaseBox] =
    Seq(DataBox(EncryProposition(contractHash), Utils.nonceFromDigest(digest ++ Ints.toByteArray(idx)), data))

  override lazy val isValid: Boolean = data.length <= Constants.MaxDataLength

  override def serializer: Serializer[M] = DataDirectiveSerializer

  override def toDbVersion(txId: ModifierId, numberInTx: Int): DirectiveDBVersion =
    DirectiveDBVersion(Base16.encode(txId), numberInTx, typeId, isValid, Base16.encode(contractHash), 0L, "", None, Base16.encode(data))
}

object DataDirective {

  val TypeId: DTypeId = 5.toByte

  implicit val jsonEncoder: Encoder[DataDirective] = (d: DataDirective) => Map(
    "typeId" -> d.typeId.asJson,
    "contractHash" -> Algos.encode(d.contractHash).asJson,
    "data" -> Algos.encode(d.data).asJson
  ).asJson

  implicit val jsonDecoder: Decoder[DataDirective] = (c: HCursor) => {
    for {
      contractHash <- c.downField("contractHash").as[String]
      dataEnc <- c.downField("data").as[String]
    } yield Algos.decode(contractHash)
      .flatMap(ch => Algos.decode(dataEnc).map(data =>  DataDirective(ch, data)))
      .getOrElse(throw new Exception("Decoding failed"))
  }
}

object DataDirectiveSerializer extends Serializer[DataDirective] {

  override def toBytes(obj: DataDirective): Array[Byte] =
    Bytes.concat(
      obj.contractHash,
      Ints.toByteArray(obj.data.length),
      obj.data
    )

  override def parseBytes(bytes: Array[Byte]): Try[DataDirective] = Try {
    val contractHash: ContractHash = bytes.take(Constants.DigestLength)
    val dataLen: Int = Ints.fromByteArray(bytes.slice(Constants.DigestLength, Constants.DigestLength + 4))
    val data: Array[DTypeId] = bytes.slice(Constants.DigestLength + 4, Constants.DigestLength + 4 + dataLen)
    DataDirective(contractHash, data)
  }
}
