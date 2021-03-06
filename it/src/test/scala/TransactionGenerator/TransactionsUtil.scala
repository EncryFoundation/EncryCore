package TransactionGenerator

import com.google.common.primitives.{Bytes, Longs}
import com.typesafe.scalalogging.StrictLogging
import org.encryfoundation.common.crypto.{PrivateKey25519, PublicKey25519, Signature25519}
import org.encryfoundation.common.modifiers.mempool.directive._
import org.encryfoundation.common.modifiers.mempool.transaction.{Input, Proof, PubKeyLockedContract, Transaction}
import org.encryfoundation.common.modifiers.state.box.{AssetBox, MonetaryBox, TokenIssuingBox}
import org.encryfoundation.common.modifiers.state.box.TokenIssuingBox.TokenId
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
        case (collectionForTransfer, (tId, element)) =>
          val directiveForChange: IndexedSeq[TransferDirective] =
            createDirectiveForChange(
              privKey,
              tId.map(_ => 0L).getOrElse(fee),
              element._2,
              element._1.map(_.amount).sum,
              tId
            )
          val collectionWithDirectiveForTransfer: IndexedSeq[TransferDirective] =
            collectionForTransfer :+ TransferDirective(recipient, element._2, tId.map(id => ADKey @@ id))
          collectionWithDirectiveForTransfer ++ directiveForChange
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
    val directiveForChange: IndexedSeq[TransferDirective] =
      createDirectiveForChange(privKey, fee, amount = 0, useOutputs.map(_._1.amount).sum)
    val directiveForScriptedAssetIssue: IndexedSeq[Directive] =
      directiveForChange :+ ScriptedAssetDirective(contract.hash, numberOfTokensForIssue, tokenIdOpt)
    prepareTransaction(privKey, fee, timestamp, useOutputs, directiveForScriptedAssetIssue)
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
    prepareTransaction(privKey, fee, timestamp, useOutputs, directiveForTokenIssue)
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
    prepareTransaction(privKey, fee, timestamp, useOutputs, directiveForDataTransaction)
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