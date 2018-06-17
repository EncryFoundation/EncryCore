package encry.modifiers.mempool

import encry.account.Address
import encry.crypto.{PrivateKey25519, PublicKey25519}
import encry.modifiers.mempool.directive.{Directive, TransferDirective}
import encry.modifiers.state.box.MonetaryBox
import encry.modifiers.state.box.proof.Signature25519
import scorex.core.transaction.box.Box.Amount
import scorex.crypto.authds.ADKey

import scala.collection.immutable

object TransactionFactory {

  def defaultPaymentTransactionScratch(privKey: PrivateKey25519,
                                       fee: Amount,
                                       timestamp: Long,
                                       useBoxes: IndexedSeq[MonetaryBox],
                                       recipient: Address,
                                       amount: Amount,
                                       tokenIdOpt: Option[ADKey] = None): EncryTransaction = {

    val pubKey: PublicKey25519 = privKey.publicImage

    val unlockers: IndexedSeq[Input] = useBoxes.map(bx => Unlocker(bx.id, None)).toIndexedSeq

    val change: Amount = useBoxes.map(_.amount).sum - (amount + fee)

    val directives: IndexedSeq[TransferDirective] = if (change > 0) {
      IndexedSeq(TransferDirective(recipient, amount, 0, tokenIdOpt), TransferDirective(pubKey.address, change, 1, tokenIdOpt))
    } else {
      IndexedSeq(TransferDirective(recipient, amount, 0, tokenIdOpt))
    }

    val signature: Signature25519 = privKey.sign(EncryTransaction.getMessageToSign(fee, timestamp, unlockers, directives))

    EncryTransaction(fee, timestamp, unlockers, directives, Some(signature))
  }

  def defaultPaymentTransactionWithMultiSig(privKey: Seq[PrivateKey25519],
                                            fee: Amount,
                                            timestamp: Long,
                                            useBoxes: IndexedSeq[MonetaryBox],
                                            recipient: Address,
                                            amount: Amount,
                                            tokenIdOpt: Option[ADKey] = None): EncryTransaction = {

    val directives: IndexedSeq[TransferDirective] = IndexedSeq(TransferDirective(recipient, amount, 0, tokenIdOpt))

    val unlockers: IndexedSeq[Input] = useBoxes.map(bx => Unlocker(bx.id, None)).toIndexedSeq

    val signature: Signature25519 = privKey.head.sign(EncryTransaction.getMessageToSign(fee, timestamp, unlockers, directives))

    EncryTransaction(fee, timestamp, unlockers, directives, Some(signature))
  }

  def defaultPaymentTransaction(accPubKey: PublicKey25519,
                                signature: Signature25519,
                                fee: Amount,
                                change: Amount,
                                timestamp: Long,
                                useBoxesIds: IndexedSeq[ADKey],
                                recipient: Address,
                                amount: Amount,
                                tokenIdOpt: Option[ADKey] = None): EncryTransaction = {

    val unlockers: immutable.IndexedSeq[Input] = useBoxesIds.map(id => Unlocker(id, None)).toIndexedSeq

    val directives: IndexedSeq[TransferDirective] = if (change > 0) {
      IndexedSeq(TransferDirective(recipient, amount, 0, tokenIdOpt), TransferDirective(accPubKey.address, change, 1, tokenIdOpt))
    } else {
      IndexedSeq(TransferDirective(recipient, amount, 0, tokenIdOpt))
    }

    EncryTransaction(fee, timestamp, unlockers, directives, Some(signature))
  }

  def coinbaseTransactionScratch(privKey: PrivateKey25519,
                                 timestamp: Long,
                                 useBoxes: Seq[MonetaryBox],
                                 fees: Amount): EncryTransaction = {

    val pubKey: PublicKey25519 = privKey.publicImage

    val unlockers: IndexedSeq[Input] = useBoxes.map(bx => Unlocker(bx.id, None)).toIndexedSeq

    val directives: IndexedSeq[Directive with Product] = IndexedSeq(TransferDirective(pubKey.address, useBoxes.map(_.amount).sum + fees, 0))

    val signature: Signature25519 = privKey.sign(EncryTransaction.getMessageToSign(0, timestamp, unlockers, directives))

    EncryTransaction(0, timestamp, unlockers, directives, Some(signature))
  }
}
