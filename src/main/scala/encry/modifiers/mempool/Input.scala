package encry.modifiers.mempool

import com.google.common.primitives.{Ints, Shorts}
import encry.modifiers.mempool.EncryTransactionSerializer.SerializationException
import encry.modifiers.mempool.EncryTransactionSerializer.SerializationException
import encry.modifiers.mempool.regcontract.RegularContract
import encry.modifiers.serialization.{BytesSerializable, Serializer}
import encry.settings.{Algos, Constants}
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import org.encryfoundation.prismlang.compiler.{CompiledContract, CompiledContractSerializer}
import scorex.crypto.authds.ADKey

import scala.util.Try

case class Input(boxId: ADKey, contract: Either[CompiledContract, RegularContract], proofs: List[Proof]) extends BytesSerializable {

  override type M = Input

  override def serializer: Serializer[M] = InputSerializer

  lazy val bytesWithoutProof: Array[Byte] = InputSerializer.toBytesWithoutProof(this)

  def isUnsigned: Boolean = proofs.isEmpty

  def realContract: CompiledContract = contract match {
    case Left(cc) => cc
    case Right(rc) => rc.contract
  }
}

object Input {

  def unsigned(boxId: ADKey, contract: CompiledContract): Input = Input(boxId, Left(contract), List.empty)
  def unsigned(boxId: ADKey, contract: RegularContract): Input = Input(boxId, Right(contract), List.empty)

  implicit val jsonEncoder: Encoder[Input] = (u: Input) => Map(
    "boxId" -> Algos.encode(u.boxId).asJson,
    "contract" -> (u.contract match {
      case l: Left[CompiledContract, RegularContract] => Algos.encode(InputSerializer.encodeEitherCompiledOrRegular(l)).asJson
      case r: Right[CompiledContract, RegularContract] => Algos.encode(InputSerializer.encodeEitherCompiledOrRegular(r)).asJson
    }),
    "proofs" -> u.proofs.map(_.asJson).asJson
  ).asJson

  implicit val jsonDecoder: Decoder[Input] = (c: HCursor) => {
    for {
      boxId <- c.downField("boxId").as[String]
      contractBytes <- c.downField("contract").as[String]
      proofs <- c.downField("proofs").as[List[Proof]]
    } yield Algos.decode(contractBytes)
      .flatMap(InputSerializer.decodeEitherCompiledOrRegular)
      .flatMap(contract => Algos.decode(boxId).map(id => Input(ADKey @@ id, contract, proofs)))
      .getOrElse(throw new Exception("Decoding failed"))
  }
}

object InputSerializer extends Serializer[Input] {

  val CCTypeId: Byte = 98
  val RCTypeId: Byte = 99

  def encodeEitherCompiledOrRegular(contract: Either[CompiledContract, RegularContract]): Array[Byte] = contract match {
    case Left(cc) => CCTypeId +: cc.bytes
    case Right(rc) => RCTypeId +: rc.bytes
  }

  def decodeEitherCompiledOrRegular(bytes: Array[Byte]): Try[Either[CompiledContract, RegularContract]] = bytes.head match {
    case CCTypeId => CompiledContractSerializer.parseBytes(bytes.tail).map(Left.apply)
    case RCTypeId => RegularContract.Serializer.parseBytes(bytes.tail).map(Right.apply)
  }

  def toBytesWithoutProof(obj: Input): Array[Byte] = {
    val contractBytes: Array[Byte] = encodeEitherCompiledOrRegular(obj.contract)
    obj.boxId ++ Ints.toByteArray(contractBytes.length) ++ contractBytes
  }

  override def toBytes(obj: Input): Array[Byte] =
    if (obj.isUnsigned) toBytesWithoutProof(obj) else {
      val proofsBytes: Array[Byte] = obj.proofs.foldLeft(Array.empty[Byte]) { case (acc, proof) =>
        val proofBytes: Array[Byte] = ProofSerializer.toBytes(proof)
        acc ++ Shorts.toByteArray(proofBytes.length.toShort) ++ proofBytes
      }
      toBytesWithoutProof(obj) ++ Array(obj.proofs.size.toByte) ++ proofsBytes
    }

  override def parseBytes(bytes: Array[Byte]): Try[Input] = Try {
    val boxId: ADKey = ADKey @@ bytes.take(Constants.ModifierIdSize)
    val contractLen: Int = Ints.fromByteArray(bytes.slice(Constants.ModifierIdSize, Constants.ModifierIdSize + 4))
    boxId -> contractLen
  }.flatMap { case (boxId, contractLen) =>
    decodeEitherCompiledOrRegular(bytes.slice(Constants.ModifierIdSize + 4, Constants.ModifierIdSize + 4 + contractLen)).map { contract =>
      val proofsQty: Int = bytes.drop(Constants.ModifierIdSize).head
      val (proofs: List[Proof], _) = (0 until proofsQty).foldLeft(List.empty[Proof], bytes.drop(Constants.ModifierIdSize + 1)) { case ((acc, bytesAcc), _) =>
        val proofLen: Int = Shorts.fromByteArray(bytesAcc.take(2))
        val proof: Proof = ProofSerializer.parseBytes(bytesAcc.slice(2, proofLen + 2)).getOrElse(throw SerializationException)
        (acc :+ proof) -> bytesAcc.drop(proofLen + 2)
      }
      Input(boxId, contract, proofs)
    }
  }
}