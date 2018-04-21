package encry.modifiers.state.box.proof
import com.google.common.primitives.Ints
import encry.modifiers.state.box.proof.Proof.ProofTypeId
import encry.settings.Algos
import encrywm.backend.env.{ESObject, ESValue}
import encrywm.lib.Types
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import scorex.core.serialization.Serializer

import scala.util.Try

case class MultiSig(proofs: Seq[Proof]) extends Proof {

  override val typeId: ProofTypeId = MultiSig.TypeId

  override val esType: Types.ESProduct = _

  override def asVal: ESValue = ???

  override def convert: ESObject = ???

  override type M = MultiSig

  override def serializer: Serializer[MultiSig.this.type] = ???
}

object MultiSig{

  val TypeId: ProofTypeId = 2.toByte

  implicit val jsonEncoder: Encoder[MultiSig] = (p: MultiSig) => Map(
    "proofs" -> p.proofs.asJson
  ).asJson

  implicit val jsonDecoder: Decoder[MultiSig] =
    (c: HCursor) => {
      for {
        proofs <- c.downField("proofs").as[Seq[Proof]]
      } yield {
        MultiSig(proofs)
      }
    }
}

object MultiProofSerializer extends Serializer[MultiSig] {

  override def toBytes(obj: MultiSig): Array[Byte] = obj.proofs.foldLeft(Ints.toByteArray(obj.proofs.length)){
    case (arr, proof) =>
      val proofSerialized = ProofSerializer.toBytes(proof)
      arr ++ Ints.toByteArray(proofSerialized.length) ++ proofSerialized
  }

  override def parseBytes(bytes: Array[ProofTypeId]): Try[MultiSig] = Try{
    val proofsQty: Int = Ints.fromByteArray(bytes.slice(0, 4))
    var startPoint: Int = 4
    val proofs = (0 until proofsQty).foldLeft(Seq[Proof]()){
      case (seq, _) =>
        val proofSize = Ints.fromByteArray(bytes.slice(startPoint, startPoint + 4))
        val proofDes = ProofSerializer.parseBytes(bytes.slice(startPoint + 4, startPoint + 4 + proofSize)).get
        startPoint = startPoint + 4 + proofSize
        seq :+ proofDes
    }
    MultiSig(proofs)
  }
}