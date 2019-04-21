package encry.modifiers.mempool

import com.typesafe.scalalogging.StrictLogging
import encry.modifiers.mempool.directive.{Directive, TransferDirective}
import encry.modifiers.state.box.Box.Amount
import encry.modifiers.state.box.MonetaryBox
import encry.view.history.History.Height
import org.encryfoundation.common.crypto.{PrivateKey25519, PublicKey25519, Signature25519}
import org.encryfoundation.common.transaction.EncryAddress.Address
import org.encryfoundation.common.transaction.{Input, Proof, PubKeyLockedContract}
import org.encryfoundation.common.utils.TaggedTypes.ADKey
import org.encryfoundation.prismlang.compiler.CompiledContract
import org.encryfoundation.prismlang.core.wrapped.BoxedValue
import scala.util.Random

object TransactionFactory extends StrictLogging {

  def defaultPaymentTransactionScratch(privKey: PrivateKey25519,
                                       fee: Amount,
                                       timestamp: Long,
                                       useBoxes: IndexedSeq[MonetaryBox],
                                       recipient: Address,
                                       amount: Amount,
                                       tokenIdOpt: Option[ADKey] = None): Transaction = {
    val pubKey: PublicKey25519 = privKey.publicImage
    val uInputs: IndexedSeq[Input] = useBoxes
      .map(bx => Input.unsigned(bx.id, Right(PubKeyLockedContract(pubKey.pubKeyBytes)))).toIndexedSeq
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
      logger.warn(s"Transaction impossible: required amount is bigger than available. Change is: $change.")
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
    val howMuchCanTransfer: Long = useOutputs.map(_._1.amount).sum - fee
    val howMuchWillTransfer: Long = howMuchCanTransfer - Math.abs(Random.nextLong % howMuchCanTransfer)
    val change: Long = howMuchCanTransfer - howMuchWillTransfer
    val directives: IndexedSeq[TransferDirective] =
      IndexedSeq(TransferDirective(recipient, howMuchWillTransfer, tokenIdOpt))
    prepareTransaction(privKey, fee, timestamp, useOutputs, directives, change, tokenIdOpt)
  }
}
