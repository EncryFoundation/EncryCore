package encry.modifiers.mempool

import encry.utils.CoreTaggedTypes.{ModifierId, ModifierTypeId}
import encry.modifiers.NodeViewModifier
import encry.modifiers.mempool.directive.{Directive, DirectiveSerializer}
import encry.modifiers.state.box.Box.Amount
import encry.modifiers.state.box.{AssetBox, DataBox}
import encry.modifiers.state.box.EncryBaseBox
import encry.EncryApp.timeProvider
import io.circe.{Decoder, HCursor}
import io.circe.syntax._
import org.encryfoundation.common.transaction._
import org.encryfoundation.prismlang.core.Types
import encry.settings.Constants
import encry.utils.CoreTaggedTypes
import io.circe.Encoder
import org.encryfoundation.common.Algos
import org.encryfoundation.common.transaction.{Input, Proof}
import org.encryfoundation.prismlang.core.PConvertible
import scorex.crypto.encode.Base16
import scorex.crypto.hash.Digest32

import scala.util.{Success, Try}
import cats.implicits._
import com.google.common.primitives.{Bytes, Longs, Shorts}
import encry.modifiers.history.Block
import encry.modifiers.history.Block.Timestamp
import encry.validation.{ModifierValidator, ValidationResult}
import encry.view.state.UtxoState
import org.encryfoundation.common.serialization.Serializer
import org.encryfoundation.common.utils.TaggedTypes.ADKey
import org.encryfoundation.prismlang.core.wrapped.{PObject, PValue}

case class Transaction(fee: Amount,
                       timestamp: Long,
                       inputs: IndexedSeq[Input],
                       directives: IndexedSeq[Directive],
                       defaultProofOpt: Option[Proof]) extends NodeViewModifier with ModifierValidator with PConvertible {

  override val modifierTypeId: ModifierTypeId = Transaction.ModifierTypeId

  override type M = Transaction

  override def serializer: Serializer[Transaction] = TransactionSerializer

  val messageToSign: Array[Byte] = UnsignedTransaction.bytesToSign(fee, timestamp, inputs, directives)

  val id: ModifierId = ModifierId !@@ Algos.hash(messageToSign)

  lazy val size: Int = this.bytes.length

  lazy val newBoxes: Traversable[EncryBaseBox] =
    directives.zipWithIndex.flatMap { case (d, idx) => d.boxes(Digest32 !@@ id, idx) }

  override def toString: String =
    s"<Transaction id=${Algos.encode(id)}\nfee=$fee\ninputs=$inputs\ndirectives=$directives\nts=$timestamp\nproofs=$defaultProofOpt>"

  //todo: Add validation of timestamp without using timeProvider from EncryApp
  lazy val semanticValidity: ValidationResult = accumulateErrors
    .demand(fee >= 0, "Negative fee amount")
    .demand(inputs.lengthCompare(inputs.toSet.size) == 0, "Inputs duplication")
    .demand(inputs.lengthCompare(Short.MaxValue) <= 0, "Wrong number of inputs")
    .demand(directives.lengthCompare(Short.MaxValue) <= 0 && directives.nonEmpty, "Wrong number of directives")
    .demand(directives.forall(_.isValid), "Invalid outputs")
    .demand(size <= Constants.PayloadMaxSize, "Invalid size")
    .result

  val tpe: Types.Product = Types.EncryTransaction

  def asVal: PValue = PValue(PObject(Map(
    "inputs" -> PValue(inputs.map(_.boxId.toList), Types.PCollection(Types.PCollection.ofByte)),
    "outputs" -> PValue(newBoxes.map(_.asPrism), Types.PCollection(Types.EncryBox)),
    "messageToSign" -> PValue(messageToSign, Types.PCollection.ofByte)
  ), tpe), tpe)
}

object Transaction {

  val ModifierTypeId: ModifierTypeId = CoreTaggedTypes.ModifierTypeId @@ 2.toByte

  case class TransactionValidationException(s: String) extends Exception(s)

  implicit val jsonEncoder: Encoder[Transaction] = (tx: Transaction) => Map(
    "id" -> Algos.encode(tx.id).asJson,
    "fee" -> tx.fee.asJson,
    "timestamp" -> tx.timestamp.asJson,
    "inputs" -> tx.inputs.map(_.asJson).asJson,
    "directives" -> tx.directives.map(_.asJson).asJson,
    "outputs" -> tx.newBoxes.toSeq.map(_.asJson).asJson,
    "defaultProofOpt" -> tx.defaultProofOpt.map(_.asJson).asJson
  ).asJson

  implicit val jsonDecoder: Decoder[Transaction] = (c: HCursor) => {
    for {
      fee <- c.downField("fee").as[Long]
      timestamp <- c.downField("timestamp").as[Long]
      inputs <- c.downField("inputs").as[IndexedSeq[Input]]
      directives <- c.downField("directives").as[IndexedSeq[Directive]]
      defaultProofOpt <- c.downField("defaultProofOpt").as[Option[Proof]]
    } yield Transaction(
      fee,
      timestamp,
      inputs,
      directives,
      defaultProofOpt
    )
  }
}

object TransactionSerializer extends Serializer[Transaction] {

  case object SerializationException extends Exception("Serialization failed.")

  override def toBytes(obj: Transaction): Array[Byte] = {
    Bytes.concat(
      Longs.toByteArray(obj.fee),
      Longs.toByteArray(obj.timestamp),
      Shorts.toByteArray(obj.inputs.size.toShort),
      Shorts.toByteArray(obj.directives.size.toShort),
      obj.inputs.map(u => Shorts.toByteArray(u.bytes.length.toShort) ++ u.bytes).foldLeft(Array[Byte]())(_ ++ _),
      obj.directives.map { d =>
        val bytes: Array[Byte] = DirectiveSerializer.toBytes(d)
        Shorts.toByteArray(bytes.length.toShort) ++ bytes
      }.reduceLeft(_ ++ _),
      obj.defaultProofOpt.map(p => ProofSerializer.toBytes(p)).getOrElse(Array.empty)
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[Transaction] = Try {
    val fee: Amount = Longs.fromByteArray(bytes.take(8))
    val timestamp: Amount = Longs.fromByteArray(bytes.slice(8, 16))
    val unlockersQty: Int = Shorts.fromByteArray(bytes.slice(16, 18))
    val directivesQty: Int = Shorts.fromByteArray(bytes.slice(18, 20))
    val leftBytes1: Array[Byte] = bytes.drop(20)
    val (unlockers: IndexedSeq[Input], unlockersLen: Int) = (0 until unlockersQty)
      .foldLeft(IndexedSeq[Input](), 0) { case ((acc, shift), _) =>
        val len: Int = Shorts.fromByteArray(leftBytes1.slice(shift, shift + 2))
        InputSerializer.parseBytes(leftBytes1.slice(shift + 2, shift + 2 + len))
          .map(u => (acc :+ u, shift + 2 + len))
          .getOrElse(throw SerializationException)
      }
    val leftBytes2: Array[Byte] = leftBytes1.drop(unlockersLen)
    val (directives: IndexedSeq[Directive], directivesLen: Int) = (0 until directivesQty)
      .foldLeft(IndexedSeq[Directive](), 0) { case ((acc, shift), _) =>
        val len: Int = Shorts.fromByteArray(leftBytes2.slice(shift, shift + 2))
        DirectiveSerializer.parseBytes(leftBytes2.slice(shift + 2, shift + 2 + len))
          .map(d => (acc :+ d, shift + 2 + len))
          .getOrElse(throw SerializationException)
      }
    val proofOpt: Option[Proof] = if (leftBytes2.length - directivesLen == 0) None else {
      ProofSerializer.parseBytes(leftBytes2.drop(directivesLen)).map(Some(_))
        .getOrElse(throw SerializationException)
    }
    Transaction(fee, timestamp, unlockers, directives, proofOpt)
  }

}

case class UnsignedTransaction(fee: Amount,
                               timestamp: Long,
                               inputs: IndexedSeq[Input],
                               directives: IndexedSeq[Directive]) {

  val messageToSign: Array[Byte] =
    UnsignedTransaction.bytesToSign(fee, timestamp, inputs, directives)

  def toSigned(proofs: IndexedSeq[Seq[Proof]], defaultProofOpt: Option[Proof]): Transaction = {
    val signedInputs: IndexedSeq[Input] = inputs.zipWithIndex.map { case (input, idx) =>
      if (proofs.nonEmpty && proofs.lengthCompare(idx + 1) <= 0) input.copy(proofs = proofs(idx).toList) else input
    }
    Transaction(fee, timestamp, signedInputs, directives, defaultProofOpt)
  }
}

object UnsignedTransaction {

  def bytesToSign(fee: Amount,
                  timestamp: Long,
                  unlockers: IndexedSeq[Input],
                  directives: IndexedSeq[Directive]): Digest32 =
    Algos.hash(Bytes.concat(
      unlockers.map(_.bytesWithoutProof).foldLeft(Array[Byte]())(_ ++ _),
      directives.map(_.bytes).foldLeft(Array[Byte]())(_ ++ _),
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee)
    ))
}


case class TransactionDBVersion(id: String, number: Int, fee: Long, blockId: String, isCoinbase: Boolean,
                                timestamp: Timestamp, proof: Option[String])

case object TransactionDBVersion {
  def apply(block: Block): Seq[TransactionDBVersion] = {
    if (block.payload.transactions.nonEmpty) {
      val transactions: Seq[TransactionDBVersion] = block.payload.transactions.zipWithIndex.map { case (tx, number) =>
        val id: String = Base16.encode(tx.id)
        val proof: Option[String] = tx.defaultProofOpt.map(p => Base16.encode(p.bytes))
        val blockId: String = Base16.encode(block.header.id)
        TransactionDBVersion(id, number, tx.fee, blockId, isCoinbase = false, tx.timestamp, proof)
      }.toIndexedSeq
      transactions match {
        case coinbase :: Nil => Seq(coinbase.copy(isCoinbase = true))
        case init :+ coinbase => init :+ coinbase.copy(isCoinbase = true)
        case Nil => throw new RuntimeException("Payload should contain at least one transaction")
      }
    } else Seq.empty
  }
}


case class InputDBVersion(id: String, txId: String, contractByteVersion: String, proofs: String, numberInTx: Int) {
  def toInput: Try[Input] =
    for {
      decodedId <- Base16.decode(id)
      decodedContractBytes <- Base16.decode(contractByteVersion)
      decodedContract      <- InputSerializer.decodeEitherCompiledOrRegular(decodedContractBytes)
      decodedBase16Proofs  <- if (proofs.length != 0) proofs.split(",").toList.traverse(Base16.decode)
                              else Success(List.empty)
      decodedProofs        <- decodedBase16Proofs.traverse(ProofSerializer.parseBytes)
    } yield {
      Input(ADKey @@ decodedId, decodedContract, decodedProofs)
    }
}

case object InputDBVersion {
  def apply(tx: Transaction): Seq[InputDBVersion] = {
    val txId: String = Base16.encode(tx.id)
    tx.inputs.zipWithIndex.map { case (in, number) =>
      val id: String = Base16.encode(in.boxId)
      val contractBytes: String = Base16.encode(InputSerializer.encodeEitherCompiledOrRegular(in.contract))
      val proofs: String = in.proofs.map(_.bytes).map(Base16.encode).mkString(",")
      InputDBVersion(id, txId, contractBytes, proofs, number)
    }
  }
}

case class OutputDBVersion(id: String, txId: String, monetaryValue: Long, coinId: String, contractHash: String, data: String)

object OutputDBVersion {
  def apply(tx: Transaction): Seq[OutputDBVersion] = {
    val txId: String = Base16.encode(tx.id)
    tx.newBoxes.map { bx =>
      val id: String = Base16.encode(bx.id)
      val (monetaryValue: Long, coinId: String, dataOpt: Option[Array[Byte]@unchecked]) = bx match {
        case ab: AssetBox => (ab.amount, Base16.encode(ab.tokenIdOpt.getOrElse(Constants.IntrinsicTokenId)), None)
        case db: DataBox => (0L, Base16.encode(Constants.IntrinsicTokenId), Some(db.data))
        case _ => (0L, Base16.encode(Constants.IntrinsicTokenId), None)
      }
      val data: String = dataOpt.map(Base16.encode).getOrElse("")
      val contractHash: String = Base16.encode(bx.proposition.contractHash)
      OutputDBVersion(id, txId, monetaryValue, coinId, contractHash, data)
    }.toIndexedSeq
  }
}

