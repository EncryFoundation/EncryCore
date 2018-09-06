package encry.modifiers.mempool

import encry.modifiers.mempool.directive.{Directive, TransferDirective}
import encry.modifiers.state.box.Box.Amount
import encry.modifiers.state.box.MonetaryBox
import encry.view.history.History.Height
import org.encryfoundation.common.crypto.{PrivateKey25519, PublicKey25519, Signature25519}
import org.encryfoundation.common.transaction.EncryAddress.Address
import org.encryfoundation.common.transaction.{Input, Proof, PubKeyLockedContract}
import org.encryfoundation.common.utils.TaggedTypes.ADKey
import org.encryfoundation.prismlang.core.wrapped.BoxedValue

object TransactionFactory {

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
}
