package encry.modifiers.state.box

import com.google.common.primitives.{Bytes, Longs}
import encry.modifiers.mempool.EncryTransaction
import encry.modifiers.state.box.EncryBox.BxTypeId
import encry.modifiers.state.box.proposition.{HeightProposition, HeightPropositionSerializer}
import encry.modifiers.state.box.serializers.SizedCompanionSerializer
import encry.settings.Algos
import io.circe.Json
import io.circe.syntax._
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Digest32

import scala.util.{Failure, Success, Try}

case class GenesisBox(override val proposition: HeightProposition,
                      override val nonce: Long,
                      genesisBytes: Array[Byte])
  extends EncryNoncedBox[HeightProposition] {

  override type M = GenesisBox

  override val typeId: BxTypeId = GenesisBox.typeId

  override lazy val bxHash: Digest32 = Algos.hash(
    Bytes.concat(
      proposition.bytes,
      Longs.toByteArray(nonce),
      genesisBytes
    )
  )

  override def unlockTry(modifier: EncryTransaction,
                         script: Option[String] = None, ctxOpt: Option[Context]): Try[Unit] =
    if (ctxOpt.isDefined && proposition.height <= ctxOpt.get.height) Success()
    else Failure(new Error("Unlock failed"))

  override def serializer: SizedCompanionSerializer[M] = GenesisBoxSerializer

  override def json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "proposition" -> s"Open after ${proposition.height}".asJson,
    "nonce" -> nonce.asJson
  ).asJson
}

object GenesisBox {

   val typeId: BxTypeId = 99.toByte
}

object GenesisBoxSerializer extends SizedCompanionSerializer[GenesisBox] {

  val Size: Int = 220

  override def toBytes(obj: GenesisBox): Array[Byte] = {
    Bytes.concat(
      obj.proposition.serializer.toBytes(obj.proposition),
      Longs.toByteArray(obj.nonce),
      obj.genesisBytes
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[GenesisBox] = Try {
    println(bytes.length)
    val proposition = HeightPropositionSerializer.parseBytes(bytes.slice(0, HeightPropositionSerializer.Size)).get
    val nonce = Longs.fromByteArray(bytes.slice(HeightPropositionSerializer.Size, HeightPropositionSerializer.Size + 8))
    val gb = bytes.slice(HeightPropositionSerializer.Size + 8, bytes.length)
    GenesisBox(proposition, nonce, gb)
  }
}
