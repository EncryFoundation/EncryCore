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
                                amount: Long,
                                tokenAmount: Long = 0): Transaction = {
    val tb: Map[TokenId, TokenIssuingBox] = useOutputs.map(_._1).collect {
      case tb: TokenIssuingBox => tb
    }.map(box => box.tokenId -> box).toMap
    val tokensDirectives: IndexedSeq[TransferDirective] =
      tb.foldLeft(IndexedSeq[TransferDirective]()) { case (directivesAll, tokenBox) =>
        directivesAll :+ TransferDirective(recipient, tokenAmount, Some(ADKey @@ tokenBox._1))
      }
    val allDirectives: IndexedSeq[TransferDirective] = tokensDirectives :+ TransferDirective(recipient, amount, None)
    prepareTransaction(privKey, fee, timestamp, useOutputs, allDirectives, amount, tokenAmount)
  }

  def scriptedAssetTransactionScratch(privKey: PrivateKey25519,
                                      fee: Long,
                                      timestamp: Long,
                                      useOutputs: Seq[(MonetaryBox, Option[(CompiledContract, Seq[Proof])])],
                                      contract: CompiledContract,
                                      amount: Long,
                                      numberOfCreatedDirectives: Int = 1,
                                      tokenIdOpt: Option[ADKey] = None): Transaction = {
    val directives: IndexedSeq[ScriptedAssetDirective] =
      (1 to numberOfCreatedDirectives).foldLeft(IndexedSeq.empty[ScriptedAssetDirective]) { case (directivesAll, _) =>
        directivesAll :+ ScriptedAssetDirective(contract.hash, amount, tokenIdOpt)
      }
    prepareTransaction(privKey, fee, timestamp, useOutputs, directives, amount)
  }

  def assetIssuingTransactionScratch(privKey: PrivateKey25519,
                                     fee: Long,
                                     timestamp: Long,
                                     useOutputs: Seq[(MonetaryBox, Option[(CompiledContract, Seq[Proof])])],
                                     contract: CompiledContract,
                                     amount: Long,
                                     numberOfCreatedDirectives: Int = 1): Transaction = {
    val directives: IndexedSeq[AssetIssuingDirective] =
      (1 to numberOfCreatedDirectives).foldLeft(IndexedSeq.empty[AssetIssuingDirective]) { case (directivesAll, _) =>
        directivesAll :+ AssetIssuingDirective(contract.hash, amount)
      }
    prepareTransaction(privKey, fee, timestamp, useOutputs, directives, amount)
  }

  def dataTransactionScratch(privKey: PrivateKey25519,
                             fee: Long,
                             timestamp: Long,
                             useOutputs: Seq[(MonetaryBox, Option[(CompiledContract, Seq[Proof])])],
                             contract: CompiledContract,
                             amount: Long,
                             data: Array[Byte],
                             numberOfCreatedDirectives: Int = 1): Transaction = {
    val directives: IndexedSeq[DataDirective] =
      (1 to numberOfCreatedDirectives).foldLeft(IndexedSeq.empty[DataDirective]) { case (directivesAll, _) =>
        directivesAll :+ DataDirective(contract.hash, data)
      }
    prepareTransaction(privKey, fee, timestamp, useOutputs, directives, amount)
  }

  private def prepareTransaction(privKey: PrivateKey25519,
                                 fee: Long,
                                 timestamp: Long,
                                 useOutputs: Seq[(MonetaryBox, Option[(CompiledContract, Seq[Proof])])],
                                 directivesSeq: IndexedSeq[Directive],
                                 amount: Long,
                                 tokenAmount: Long = 0): Transaction = {

    val pubKey: PublicKey25519     = privKey.publicImage
    val uInputs: IndexedSeq[Input] = useOutputs.toIndexedSeq.map { case (box, contractOpt) =>
      Input.unsigned(
        box.id,
        contractOpt match {
          case Some((ct, _)) => Left(ct)
          case None          => Right(PubKeyLockedContract(pubKey.pubKeyBytes))
        }
      )}

    val tb: Map[TokenId, TokenIssuingBox] = useOutputs.map(_._1).collect {
      case tb: TokenIssuingBox => tb
    }.map(box => box.tokenId -> box).toMap
    val ab: Seq[AssetBox] = useOutputs.map(_._1).collect { case ab: AssetBox => ab }

    val tokensChange = tb.foldLeft(Seq[(TokenId, Long)]()) { case (seq, tokens) =>
      (tokens._1 -> (tokens._2.amount - tokenAmount)) +: seq
    }

    val coinChange = ab.map(_.amount).sum - (amount + fee)

    tokensChange.foreach(x => if (x._2 < 0) {
      logger.warn(s"Transaction impossible: required amount is bigger than available. Change is: ${x._2}.")
      throw new RuntimeException(s"Transaction impossible: required amount is bigger than available ${x._2}")
    })

    if (coinChange < 0) {
      logger.warn(s"Transaction impossible: required amount is bigger than available. Change is: $coinChange.")
      throw new RuntimeException(s"Transaction impossible: required amount is bigger than available $coinChange")
    }

    val directivesForEncry: IndexedSeq[Directive] =
      directivesSeq :+ TransferDirective(pubKey.address.address, coinChange, None)

    val directivesForTokens: IndexedSeq[Directive] = tokensChange.foldLeft(directivesForEncry) {
      case (seq, anotherAmount) =>
        seq :+ TransferDirective(
          pubKey.address.address,
          anotherAmount._2,
          Some(ADKey @@ anotherAmount._1)
        )
    }

    val uTransaction: UnsignedEncryTransaction = UnsignedEncryTransaction(fee, timestamp, uInputs, directivesForTokens)
    val signature: Signature25519              = privKey.sign(uTransaction.messageToSign)
    val proofs: IndexedSeq[Seq[Proof]]         = useOutputs.flatMap(_._2.map(_._2)).toIndexedSeq

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