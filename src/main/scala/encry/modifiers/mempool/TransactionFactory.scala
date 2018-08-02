package encry.modifiers.mempool

import encry.Address
import encry.crypto.{PrivateKey25519, PublicKey25519, Signature25519}
import encry.modifiers.mempool.directive.{Directive, TransferDirective}
import encry.modifiers.mempool.regcontract.PubKeyLockedContract
import encry.modifiers.state.box.Box.Amount
import encry.modifiers.state.box.MonetaryBox
import encry.view.history.Height
import org.encryfoundation.prismlang.core.wrapped.BoxedValue
import scorex.crypto.authds.ADKey

object TransactionFactory {

  def defaultPaymentTransactionScratch(privKey: PrivateKey25519,
                                       fee: Amount,
                                       timestamp: Long,
                                       useBoxes: IndexedSeq[MonetaryBox],
                                       recipient: Address,
                                       amount: Amount,
                                       tokenIdOpt: Option[ADKey] = None): EncryTransaction = {
    val pubKey: PublicKey25519 = privKey.publicImage
    val uInputs: IndexedSeq[Input] = useBoxes.map(bx => Input.unsigned(bx.id, PubKeyLockedContract(pubKey.pubKeyBytes))).toIndexedSeq
    val change: Amount = useBoxes.map(_.amount).sum - (amount + fee)
    val directives: IndexedSeq[TransferDirective] =
      if (change > 0) IndexedSeq(TransferDirective(recipient, amount, tokenIdOpt), TransferDirective(pubKey.address.address, change, tokenIdOpt))
      else IndexedSeq(TransferDirective(recipient, amount, tokenIdOpt))

    val uTransaction: UnsignedEncryTransaction = UnsignedEncryTransaction(fee, timestamp, uInputs, directives)
    val signature: Signature25519 = privKey.sign(uTransaction.messageToSign)

    uTransaction.toSigned(IndexedSeq.empty, Some(Proof(BoxedValue.Signature25519Value(signature.bytes.toList))))
  }

  def coinbaseTransactionScratch(pubKey: PublicKey25519,
                                 timestamp: Long,
                                 supply: Amount,
                                 amount: Amount,
                                 height: Height): EncryTransaction = {
    val directives: IndexedSeq[Directive with Product] =
      IndexedSeq(TransferDirective(pubKey.address.address, amount + supply))

    EncryTransaction(0, timestamp, IndexedSeq.empty, directives, None)
  }
}
