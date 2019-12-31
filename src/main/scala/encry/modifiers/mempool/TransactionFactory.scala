package encry.modifiers.mempool

import cats.Applicative
import cats.implicits._
import com.google.common.primitives.{Bytes, Longs}
import com.typesafe.scalalogging.StrictLogging
import org.encryfoundation.common.crypto.{PrivateKey25519, PublicKey25519, Signature25519}
import org.encryfoundation.common.modifiers.mempool.directive.{AssetIssuingDirective, DataDirective, Directive, ScriptedAssetDirective, TransferDirective}
import org.encryfoundation.common.modifiers.mempool.transaction.EncryAddress.Address
import org.encryfoundation.common.modifiers.mempool.transaction._
import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.modifiers.state.box.{AssetBox, MonetaryBox, TokenIssuingBox}
import org.encryfoundation.common.modifiers.state.box.TokenIssuingBox.TokenId
import org.encryfoundation.common.utils.TaggedTypes.{ADKey, Height}
import org.encryfoundation.prismlang.compiler.{CompiledContract, CompiledContractSerializer, PCompiler}
import org.encryfoundation.prismlang.core.wrapped.BoxedValue
import scorex.crypto.hash.{Blake2b256, Digest32}

import scala.util.{Failure, Success, Try}

object TransactionFactory extends StrictLogging {

  def defaultPaymentTransactionScratch(privKey: PrivateKey25519,
                                       fee: Amount,
                                       timestamp: Long,
                                       useBoxes: IndexedSeq[MonetaryBox],
                                       recipient: Address,
                                       amount: Amount,
                                       tokenIdOpt: Option[ADKey] = None): Transaction = {
    val pubKey: PublicKey25519 = privKey.publicImage
    val uInputs: IndexedSeq[Input] = useBoxes.map(bx =>
      Input.unsigned(bx.id, Right(PubKeyLockedContract(pubKey.pubKeyBytes)))
    ).toIndexedSeq
    val change: Amount = useBoxes.map(_.amount).sum - (amount + fee)
    val directives: IndexedSeq[TransferDirective] =
      if (change > 0) IndexedSeq(
        TransferDirective(recipient, amount, tokenIdOpt), TransferDirective(pubKey.address.address, change, tokenIdOpt)
      )
      else IndexedSeq(TransferDirective(recipient, amount, tokenIdOpt))

    val uTransaction: UnsignedTransaction = UnsignedTransaction(fee, timestamp, uInputs, directives)
    val signature: Signature25519 = privKey.sign(uTransaction.messageToSign)

    uTransaction.toSigned(IndexedSeq.empty, Some(Proof(BoxedValue.Signature25519Value(signature.bytes.toList))))
  }

  def dataTransactionScratch(privKey: PrivateKey25519,
                             fee: Long,
                             timestamp: Long,
                             useOutputs: Seq[(MonetaryBox, Option[(CompiledContract, Seq[Proof])])],
                             contract: CompiledContract,
                             data: Array[Byte]): Transaction = {
    val directiveForChange: IndexedSeq[TransferDirective] =
      createDirectiveForChange(privKey, fee, amount = 0, useOutputs.map(_._1.amount).sum)
    val directiveForDataTransaction: IndexedSeq[Directive] =
      directiveForChange :+ DataDirective(contract.hash, data)
    prepareTransaction1(privKey, fee, timestamp, useOutputs, directiveForDataTransaction)
  }

  def coinbaseTransactionScratch(pubKey: PublicKey25519,
                                 timestamp: Long,
                                 supply: Amount,
                                 amount: Amount,
                                 height: Height): Transaction = {
    val directives: IndexedSeq[Directive with Product] =
      IndexedSeq(TransferDirective(pubKey.address.address, amount + supply))

    Transaction(0, timestamp, IndexedSeq.empty, directives, None)
  }

  def prepareTransaction(privKey: PrivateKey25519,
                         fee: Long,
                         timestamp: Long,
                         useOutputs: Seq[(MonetaryBox, Option[(CompiledContract, Seq[Proof])])],
                         directivesSeq: IndexedSeq[Directive],
                         amount: Long,
                         tokenIdOpt: Option[ADKey] = None): Transaction = {

    val pubKey: PublicKey25519 = privKey.publicImage

    val uInputs: IndexedSeq[Input] = useOutputs.toIndexedSeq.map { case (box, contractOpt) =>
      Input.unsigned(
        box.id,
        contractOpt match {
          case Some((ct, _)) => Left(ct)
          case None => Right(PubKeyLockedContract(pubKey.pubKeyBytes))
        }
      )
    }

    val change: Long = amount

    if (change < 0) {
      logger.info(s"Transaction impossible: required amount is bigger than available. Change is: $change.")
      throw new RuntimeException("Transaction impossible: required amount is bigger than available.")
    }

    val directives: IndexedSeq[Directive] =
      if (change > 0) directivesSeq ++: IndexedSeq(TransferDirective(pubKey.address.address, change, tokenIdOpt))
      else directivesSeq

    val uTransaction: UnsignedTransaction = UnsignedTransaction(fee, timestamp, uInputs, directives)
    val signature: Signature25519         = privKey.sign(uTransaction.messageToSign)
    val proofs: IndexedSeq[Seq[Proof]]    = useOutputs.flatMap(_._2.map(_._2)).toIndexedSeq

    uTransaction.toSigned(proofs, Some(Proof(BoxedValue.Signature25519Value(signature.bytes.toList))))
  }

  def defaultPaymentTransaction(privKey: PrivateKey25519,
                                fee: Long,
                                timestamp: Long,
                                useOutputs: Seq[(MonetaryBox, Option[(CompiledContract, Seq[Proof])])],
                                recipient: String,
                                amount: Long,
                                tokenIdOpt: Option[ADKey] = None): Transaction = {
    val howMuchCanTransfer: Long =
      if (tokenIdOpt.isEmpty) useOutputs.map(_._1.amount).sum - fee
      else useOutputs.map(_._1).collect {
        case ab: AssetBox if Applicative[Option].map2(ab.tokenIdOpt, tokenIdOpt)(_.sameElements(_)).getOrElse(false) => ab
        case tib: TokenIssuingBox if tokenIdOpt.exists(_.sameElements(tib.tokenId)) => tib
      }.map(_.amount).sum
    val change: Long = howMuchCanTransfer - amount
    val directives: IndexedSeq[TransferDirective] =
      IndexedSeq(TransferDirective(recipient, amount, tokenIdOpt))
    prepareTransaction(privKey, fee, timestamp, useOutputs, directives, change, tokenIdOpt)
  }

  def defaultContractTransaction(privKey: PrivateKey25519,
                                fee: Long,
                                timestamp: Long,
                                useOutputs: Seq[(MonetaryBox, Option[(CompiledContract, Seq[Proof])])],
                                 contract: String,
                                amount: Long,
                                tokenIdOpt: Option[ADKey] = None): Transaction = {
    val compiledContract = PCompiler.compile(contract).get.hash
    val howMuchCanTransfer: Long =
      if (tokenIdOpt.isEmpty) useOutputs.map(_._1.amount).sum - fee
      else useOutputs.map(_._1).collect {
        case ab: AssetBox if Applicative[Option].map2(ab.tokenIdOpt, tokenIdOpt)(_.sameElements(_)).getOrElse(false) => ab
        case tib: TokenIssuingBox if tokenIdOpt.exists(_.sameElements(tib.tokenId)) => tib
      }.map(_.amount).sum
    val change: Long = howMuchCanTransfer - amount
    val directives: IndexedSeq[ScriptedAssetDirective] =
      IndexedSeq(ScriptedAssetDirective(compiledContract, amount, tokenIdOpt))
    prepareTransaction(privKey, fee, timestamp, useOutputs, directives, change, tokenIdOpt)
  }

  def assetIssuingTransactionScratch(privKey: PrivateKey25519,
                                     fee: Long,
                                     timestamp: Long,
                                     useOutputs: Seq[(MonetaryBox, Option[(CompiledContract, Seq[Proof])])],
                                     contract: CompiledContract,
                                     numberOfTokensForIssue: Long): Transaction = {
    val directiveForChange: IndexedSeq[TransferDirective] =
      createDirectiveForChange(privKey, fee, amount = 0, useOutputs.map(_._1.amount).sum)
    val directiveForTokenIssue: IndexedSeq[Directive] =
      directiveForChange :+ AssetIssuingDirective(contract.hash, numberOfTokensForIssue)
    prepareTransaction1(privKey, fee, timestamp, useOutputs, directiveForTokenIssue)
  }

  private def prepareTransaction1(privKey: PrivateKey25519,
                                 fee: Long,
                                 timestamp: Long,
                                 useOutputs: Seq[(MonetaryBox, Option[(CompiledContract, Seq[Proof])])],
                                 directivesForTransfer: IndexedSeq[Directive]): Transaction = {

    val pubKey: PublicKey25519 = privKey.publicImage
    val uInputs: IndexedSeq[Input] = useOutputs.toIndexedSeq.map { case (box, contractOpt) =>
      Input.unsigned(
        box.id,
        contractOpt match {
          case Some((ct, _)) => Left(ct)
          case None => Right(PubKeyLockedContract(pubKey.pubKeyBytes))
        }
      )
    }

    val uTransaction: UnsignedEncryTransaction = UnsignedEncryTransaction(fee, timestamp, uInputs, directivesForTransfer)
    val signature: Signature25519 = privKey.sign(uTransaction.messageToSign)
    val proofs: IndexedSeq[Seq[Proof]] = useOutputs.flatMap(_._2.map(_._2)).toIndexedSeq

    uTransaction.toSigned(proofs, Some(Proof(BoxedValue.Signature25519Value(signature.bytes.toList))))
  }

  private def createDirectiveForChange(privKey: PrivateKey25519,
                                       fee: Long,
                                       amount: Long,
                                       totalAmount: Long,
                                       tokenId: Option[TokenId] = None): IndexedSeq[TransferDirective] = {
    val change: Long = totalAmount - (amount + fee)
    if (change < 0) {
      logger.warn(s"Transaction impossible: required amount is bigger than available. Change is: $change.")
      throw new RuntimeException(s"Transaction impossible: required amount is bigger than available $change")
    }
    if (change > 0)
      IndexedSeq(TransferDirective(privKey.publicImage.address.address, change, tokenId.map(element => ADKey @@ element)))
    else IndexedSeq()
  }

  case class UnsignedEncryTransaction(fee: Long,
                                      timestamp: Long,
                                      inputs: IndexedSeq[Input],
                                      directives: IndexedSeq[Directive]) {

    val messageToSign: Array[Byte] = UnsignedEncryTransaction.bytesToSign(fee, timestamp, inputs, directives)

    def toSigned(proofs: IndexedSeq[Seq[Proof]], defaultProofOpt: Option[Proof]): Transaction = {
      val signedInputs: IndexedSeq[Input] = inputs.zipWithIndex.map { case (input, idx) =>
        if (proofs.nonEmpty && proofs.lengthCompare(idx + 1) <= 0) input.copy(proofs = proofs(idx).toList) else input
      }
      Transaction(fee, timestamp, signedInputs, directives, defaultProofOpt)
    }
  }

  object UnsignedEncryTransaction {

    def bytesToSign(fee: Long,
                    timestamp: Long,
                    inputs: IndexedSeq[Input],
                    directives: IndexedSeq[Directive]): Digest32 =
      Blake2b256.hash(Bytes.concat(
        inputs.flatMap(_.bytesWithoutProof).toArray,
        directives.flatMap(_.bytes).toArray,
        Longs.toByteArray(timestamp),
        Longs.toByteArray(fee)
      ))
  }
}