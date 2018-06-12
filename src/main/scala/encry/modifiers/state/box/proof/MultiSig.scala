package encry.modifiers.state.box.proof
import com.google.common.primitives.Ints
import encry.modifiers.state.box.proof.Proof.ProofTypeId
import org.encryfoundation.prismlang.core.Types
import org.encryfoundation.prismlang.core.wrapped.{PObject, PValue}
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import scorex.core.serialization.Serializer

import scala.util.Try

case class MultiSig(proofs: Seq[Signature25519]) extends Proof {

  override type M = MultiSig

  override val typeId: ProofTypeId = MultiSig.TypeId

  override val esType: Types.Product = Types.MultiSig

  override def asVal: PValue = PValue(Types.MultiSig)(convert)

  override def convert: PObject = {
    val fields = Map(
      "typeId" -> PValue(Types.PInt)(typeId.toInt),
      "proofs" -> PValue(Types.PCollection(Types.Signature25519))(proofs.map(_.convert).toList)
    )
    PObject(fields, esType)
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