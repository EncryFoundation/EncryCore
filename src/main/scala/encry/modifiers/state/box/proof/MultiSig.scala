package encry.modifiers.state.box.proof
import com.google.common.primitives.Ints
import encry.modifiers.state.box.proof.Proof.ProofTypeId
import encrywm.lang.backend.env.{ESObject, ESValue}
import encrywm.lib.Types
import encrywm.lib.Types.{ESInt, ESList}
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import scorex.core.serialization.Serializer

import scala.util.Try

case class MultiSig(proofs: Seq[Signature25519]) extends Proof {

  override type M = MultiSig

  override val typeId: ProofTypeId = MultiSig.TypeId

  override val esType: Types.ESProduct = Types.MultiSig

  override def asVal: ESValue = ESValue(Types.MultiSig.ident.toLowerCase, Types.MultiSig)(convert)

  override def convert: ESObject = {
    val fields = Map(
      "typeId" -> ESValue("typeId", ESInt)(typeId.toInt),
      "proofs" -> ESValue("proofs", ESList(Types.Signature25519))(proofs.map(_.convert).toList)
    )
    ESObject(Types.MultiSig.ident, fields, esType)
  }

  override def serializer: Serializer[MultiSig] = MultiProofSerializer
}

object MultiSig {

  val TypeId: ProofTypeId = 2.toByte

  implicit val jsonEncoder: Encoder[MultiSig] = (p: MultiSig) => Map(
    "proofs" -> p.proofs.asJson
  ).asJson

  implicit val jsonDecoder: Decoder[MultiSig] = (c: HCursor) => {
      for {
        proofs <- c.downField("proofs").as[Seq[Signature25519]]
      } yield {
        MultiSig(proofs)
      }
    }
}

object MultiProofSerializer extends Serializer[MultiSig] {

  override def toBytes(obj: MultiSig): Array[Byte] = obj.proofs.foldLeft(Ints.toByteArray(obj.proofs.length)){
    case (arr, proof) =>
      val proofSerialized = Signature25519Serializer.toBytes(proof)
      arr ++ Ints.toByteArray(proofSerialized.length) ++ proofSerialized
  }

  override def parseBytes(bytes: Array[ProofTypeId]): Try[MultiSig] = Try{
    val proofsQty: Int = Ints.fromByteArray(bytes.slice(0, 4))
    var startPoint: Int = 4
    val proofs = (0 until proofsQty).foldLeft(Seq[Signature25519]()){
      case (seq, _) =>
        val proofSize = Ints.fromByteArray(bytes.slice(startPoint, startPoint + 4))
        val proofDes = Signature25519Serializer.parseBytes(bytes.slice(startPoint + 4, startPoint + 4 + proofSize)).get
        startPoint = startPoint + 4 + proofSize
        seq :+ proofDes
    }
    MultiSig(proofs)
  }
}