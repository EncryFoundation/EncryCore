package TransactionGenerator

import com.google.common.primitives.{Bytes, Longs}
import com.typesafe.scalalogging.StrictLogging
import encry.modifiers.mempool.Transaction
import encry.modifiers.mempool.directive._
import encry.modifiers.state.box.TokenIssuingBox.TokenId
import encry.modifiers.state.box.{AssetBox, MonetaryBox, TokenIssuingBox}
import org.encryfoundation.common.crypto.{PrivateKey25519, PublicKey25519, Signature25519}
import org.encryfoundation.common.transaction.{Input, Proof, PubKeyLockedContract}
import org.encryfoundation.common.utils.TaggedTypes.ADKey
import org.encryfoundation.prismlang.compiler.CompiledContract
import org.encryfoundation.prismlang.core.wrapped.BoxedValue
import scorex.crypto.hash.{Blake2b256, Digest32}

object CreateTransaction extends StrictLogging {

  def defaultPaymentTransaction(privKey: PrivateKey25519,
                                fee: Long,
                                timestamp: Long,
                                useOutputs: Seq[(MonetaryBox, Option[(CompiledContract, Seq[Proof])])],
                                recipient: String,
                                encryCoinAmount: Long,
                                tokensAmount: Map[TokenId, Long] = Map()): Transaction = {

    val filteredBoxes: Map[Option[TokenId], (List[MonetaryBox], Long)] =
      useOutputs.map(_._1).foldLeft(Map[Option[TokenId], (List[MonetaryBox], Long)]()) {
        case (resultCollection, box) =>
          box match {
            case tib: TokenIssuingBox =>
              resultCollection.updated(
                Some(tib.tokenId),
                (List(tib), tokensAmount.filter(elem => tib.tokenId.sameElements(elem._1)).head._2)
              )
            case ab: AssetBox =>
              val currentBoxesByTokenId: Option[(List[MonetaryBox], Long)] = resultCollection.get(ab.tokenIdOpt)
              currentBoxesByTokenId match {
                case Some(collection) => resultCollection.updated(ab.tokenIdOpt, (collection._1 :+ ab, collection._2))
                case None =>
                  val neededAmount: Long = ab.tokenIdOpt match {
                    case Some(tokenId) => tokensAmount.filter(elem => tokenId.sameElements(elem._1)).head._2
                    case None => encryCoinAmount
                  }
                  resultCollection.updated(ab.tokenIdOpt, (List(ab), neededAmount))
              }
          }
      }

    val directivesForTransferAndChangeForAllCoins: IndexedSeq[TransferDirective] =
      filteredBoxes.foldLeft(IndexedSeq[TransferDirective]()) {
        case (collectionForTransfer, element) =>
          element._1 match {
            case Some(tokenId) =>
              val directiveForChange: TransferDirective =
                createDirectiveForChange(privKey, 0, element._2._2, element._2._1.map(_.amount).sum, Some(tokenId))
              val collectionWithTransferD =
                collectionForTransfer :+ TransferDirective(recipient, element._2._2, Option(ADKey @@ tokenId))
              collectionWithTransferD :+ directiveForChange
            case None =>
              val directiveForChange: TransferDirective =
                createDirectiveForChange(privKey, fee, element._2._2, element._2._1.map(_.amount).sum, None)
              val collectionWithTransferD =
                collectionForTransfer :+ TransferDirective(recipient, element._2._2, None)
              collectionWithTransferD :+ directiveForChange
          }
      }

    prepareTransaction(
      privKey,
      fee,
      timestamp,
      useOutputs,
      directivesForTransferAndChangeForAllCoins
    )
  }

  def scriptedAssetTransactionScratch(privKey: PrivateKey25519,
                                      fee: Long,
                                      timestamp: Long,
                                      useOutputs: Seq[(MonetaryBox, Option[(CompiledContract, Seq[Proof])])],
                                      contract: CompiledContract,
                                      numberOfTokensForIssue: Long,
                                      tokenIdOpt: Option[ADKey] = None): Transaction = {
    val directiveForChange: TransferDirective =
      createDirectiveForChange(privKey, fee, amount = 0, useOutputs.map(_._1.amount).sum)
    val directiveForScriptedAssetIssue: IndexedSeq[Directive] =
      IndexedSeq(ScriptedAssetDirective(contract.hash, numberOfTokensForIssue, tokenIdOpt)) :+ directiveForChange
    prepareTransaction(privKey, fee, timestamp, useOutputs, directiveForScriptedAssetIssue)
  }

  def assetIssuingTransactionScratch(privKey: PrivateKey25519,
                                     fee: Long,
                                     timestamp: Long,
                                     useOutputs: Seq[(MonetaryBox, Option[(CompiledContract, Seq[Proof])])],
                                     contract: CompiledContract,
                                     numberOfTokensForIssue: Long): Transaction = {
    val directiveForChange: TransferDirective =
      createDirectiveForChange(privKey, fee, amount = 0, useOutputs.map(_._1.amount).sum)
    val directiveForTokenIssue: IndexedSeq[Directive] =
      IndexedSeq(AssetIssuingDirective(contract.hash, numberOfTokensForIssue)) :+ directiveForChange
    prepareTransaction(privKey, fee, timestamp, useOutputs, directiveForTokenIssue)
  }

  def dataTransactionScratch(privKey: PrivateKey25519,
                             fee: Long,
                             timestamp: Long,
                             useOutputs: Seq[(MonetaryBox, Option[(CompiledContract, Seq[Proof])])],
                             contract: CompiledContract,
                             data: Array[Byte]): Transaction = {
    val directiveForChange: TransferDirective =
      createDirectiveForChange(privKey, fee, amount = 0, useOutputs.map(_._1.amount).sum)
    val directiveForDataTransaction: IndexedSeq[Directive] =
      IndexedSeq(DataDirective(contract.hash, data)) :+ directiveForChange
    prepareTransaction(privKey, fee, timestamp, useOutputs, directiveForDataTransaction)
  }

  private def createDirectiveForChange(privKey: PrivateKey25519,
                                       fee: Long,
                                       amount: Long,
                                       totalAmount: Long,
                                       tokenId: Option[TokenId] = None): TransferDirective = {
    val change: Long = totalAmount - (amount + fee)
    if (change < 0) {
      logger.warn(s"Transaction impossible: required amount is bigger than available. Change is: $change.")
      throw new RuntimeException(s"Transaction impossible: required amount is bigger than available $change")
    }
    TransferDirective(privKey.publicImage.address.address, change, tokenId.map(element => ADKey @@ element))
  }

  private def prepareTransaction(privKey: PrivateKey25519,
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