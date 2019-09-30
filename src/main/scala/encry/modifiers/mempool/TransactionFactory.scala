package encry.modifiers.mempool

import com.google.common.primitives.{Bytes, Longs}
import com.typesafe.scalalogging.StrictLogging
import org.encryfoundation.common.crypto.{PrivateKey25519, PublicKey25519, Signature25519}
import org.encryfoundation.common.modifiers.mempool.directive.{AssetIssuingDirective, DataDirective, Directive, ScriptedAssetDirective, TransferDirective}
import org.encryfoundation.common.modifiers.mempool.transaction.EncryAddress.Address
import org.encryfoundation.common.modifiers.mempool.transaction._
import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.modifiers.state.box.MonetaryBox
import org.encryfoundation.common.utils.TaggedTypes.{ADKey, Height}
import org.encryfoundation.prismlang.compiler.CompiledContract
import org.encryfoundation.prismlang.core.wrapped.BoxedValue
import scorex.crypto.hash.{Blake2b256, Digest32}

import scala.util.Random

object TransactionFactory extends StrictLogging {

  def coinbaseTransactionScratch(pubKey: PublicKey25519,
                                 timestamp: Long,
                                 supply: Amount,
                                 amount: Amount,
                                 height: Height): Transaction = {
    val directives: IndexedSeq[Directive with Product] =
      IndexedSeq(TransferDirective(pubKey.address.address, amount + supply))

    Transaction(0, timestamp, IndexedSeq.empty, directives, None)
  }

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

  def paymentTransactionWithMultipleOutputs(privKey: PrivateKey25519, fee: Amount, timestamp: Long, useBoxes: IndexedSeq[MonetaryBox],
                                            recipient: Address, amount: Amount, tokenIdOpt: Option[ADKey] = None,
                                            numOfOutputs: Int): Transaction = {

    val pubKey: PublicKey25519 = privKey.publicImage
    val uInputs: IndexedSeq[Input] = useBoxes
      .map(bx => Input.unsigned(bx.id, Right(PubKeyLockedContract(pubKey.pubKeyBytes))))
      .toIndexedSeq

    val change: Amount = useBoxes.map(_.amount).sum - (amount + fee)
    val directives: IndexedSeq[TransferDirective] =
      if (change > 0) TransferDirective(recipient, amount, tokenIdOpt) +: (0 until numOfOutputs).map(_ =>
        TransferDirective(pubKey.address.address, change / numOfOutputs, tokenIdOpt))
      else IndexedSeq(TransferDirective(recipient, amount, tokenIdOpt))

    val uTransaction: UnsignedTransaction = UnsignedTransaction(fee, timestamp, uInputs, directives)
    val signature: Signature25519 = privKey.sign(uTransaction.messageToSign)
    uTransaction.toSigned(IndexedSeq.empty, Some(Proof(BoxedValue.Signature25519Value(signature.bytes.toList))))
  }

  def dataTransactionScratch(privKey: PrivateKey25519,
                             fee: Long,
                             timestamp: Long,
                             useOutputs: IndexedSeq[MonetaryBox],
                             amount: Long,
                             data: Array[Byte],
                             numOfOutputs: Int = 5): Transaction = {

    val pubKey: PublicKey25519 = privKey.publicImage

    val uInputs: IndexedSeq[Input] = useOutputs
      .map(bx => Input.unsigned(bx.id, Right(PubKeyLockedContract(pubKey.pubKeyBytes))))
      .toIndexedSeq

    val change: Amount = useOutputs.map(_.amount).sum - (amount + fee)

    val directives: IndexedSeq[DataDirective] =
      (0 until numOfOutputs).foldLeft(IndexedSeq.empty[DataDirective]) { case (directivesAll, _) =>
        directivesAll :+ DataDirective(PubKeyLockedContract(privKey.publicImage.pubKeyBytes).contract.hash, data)
      }

    val newDirectives: IndexedSeq[Directive] =
      if (change > 0) TransferDirective(pubKey.address.address, amount, None) +: (0 until numOfOutputs).map(_ =>
        TransferDirective(pubKey.address.address, change / numOfOutputs, None)) ++: directives
      else directives

    val uTransaction: UnsignedTransaction = UnsignedTransaction(fee, timestamp, uInputs, newDirectives)
    val signature: Signature25519 = privKey.sign(uTransaction.messageToSign)
    uTransaction.toSigned(IndexedSeq.empty, Some(Proof(BoxedValue.Signature25519Value(signature.bytes.toList))))
  }

  def assetIssuingTransactionScratch(privKey: PrivateKey25519,
                                     fee: Long,
                                     timestamp: Long,
                                     useOutputs: IndexedSeq[MonetaryBox],
                                     amount: Long,
                                     numOfOutputs: Int = 5): Transaction = {
    val directives: IndexedSeq[AssetIssuingDirective] =
      (0 until numOfOutputs).foldLeft(IndexedSeq.empty[AssetIssuingDirective]) { case (directivesAll, _) =>
        directivesAll :+ AssetIssuingDirective(PubKeyLockedContract(privKey.publicImage.pubKeyBytes).contract.hash, amount)
      }

    val pubKey: PublicKey25519 = privKey.publicImage

    val uInputs: IndexedSeq[Input] = useOutputs
      .map(bx => Input.unsigned(bx.id, Right(PubKeyLockedContract(pubKey.pubKeyBytes))))
      .toIndexedSeq

    val change: Amount = useOutputs.map(_.amount).sum - (amount + fee)

    val newDirectives: IndexedSeq[Directive] =
      if (change > 0) TransferDirective(pubKey.address.address, amount, None) +: (0 until numOfOutputs).map(_ =>
        TransferDirective(pubKey.address.address, change / numOfOutputs, None)) ++: directives
      else directives

    val uTransaction: UnsignedTransaction = UnsignedTransaction(fee, timestamp, uInputs, newDirectives)
    val signature: Signature25519 = privKey.sign(uTransaction.messageToSign)
    uTransaction.toSigned(IndexedSeq.empty, Some(Proof(BoxedValue.Signature25519Value(signature.bytes.toList))))
  }

}