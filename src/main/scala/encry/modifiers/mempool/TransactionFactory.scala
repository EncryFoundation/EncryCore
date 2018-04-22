package encry.modifiers.mempool

import encry.account.Address
import encry.crypto.{PrivateKey25519, PublicKey25519}
import encry.modifiers.mempool.directive.{AssetIssuingDirective, CoinbaseDirective, TransferDirective}
import encry.modifiers.state.box.MonetaryBox
import encry.modifiers.state.box.proof.{MultiSig, Proof, Signature25519}
import encry.view.history.Height
import encrywm.common.EncryContract
import scorex.core.transaction.box.Box.Amount
import scorex.crypto.authds.ADKey

object TransactionFactory {

  def defaultPaymentTransactionScratch(privKey: PrivateKey25519,
                                       fee: Amount,
                                       timestamp: Long,
                                       useBoxes: IndexedSeq[MonetaryBox],
                                       recipient: Address,
                                       amount: Amount,
                                       tokenIdOpt: Option[ADKey] = None): EncryTransaction = {

    val pubKey = privKey.publicImage

    val unlockers = useBoxes.map(bx => Unlocker(bx.id, None)).toIndexedSeq

    val change = useBoxes.map(_.amount).sum - (amount + fee)

    val directives = if (change > 0) {
      IndexedSeq(TransferDirective(recipient, amount, 0, tokenIdOpt), TransferDirective(pubKey.address, change, 1, tokenIdOpt))
    } else {
      IndexedSeq(TransferDirective(recipient, amount, 0, tokenIdOpt))
    }

    val signature = privKey.sign(EncryTransaction.getMessageToSign(pubKey, fee, timestamp, unlockers, directives))

    EncryTransaction(pubKey, fee, timestamp, signature, unlockers, directives)
  }

  def defaultPaymentTransactionWithMultiSig(privKey: Seq[PrivateKey25519],
                                            fee: Amount,
                                            timestamp: Long,
                                            useBoxes: IndexedSeq[MonetaryBox],
                                            recipient: Address,
                                            amount: Amount,
                                            tokenIdOpt: Option[ADKey] = None): EncryTransaction = {

    val pubKey = privKey.head.publicImage

    val directives = IndexedSeq(TransferDirective(recipient, amount, 0, tokenIdOpt))

    val unlockers = useBoxes.map(bx => Unlocker(bx.id, None)).toIndexedSeq

    val signature = privKey.head.sign(EncryTransaction.getMessageToSign(pubKey, fee, timestamp, unlockers, directives))

    EncryTransaction(pubKey, fee, timestamp, signature, unlockers, directives)
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

    val unlockers = useBoxesIds.map(id => Unlocker(id, None)).toIndexedSeq

    val directives = if (change > 0) {
      IndexedSeq(TransferDirective(recipient, amount, 0, tokenIdOpt), TransferDirective(accPubKey.address, change, 1, tokenIdOpt))
    } else {
      IndexedSeq(TransferDirective(recipient, amount, 0, tokenIdOpt))
    }

    EncryTransaction(accPubKey, fee, timestamp, signature, unlockers, directives)
  }

  def coinbaseTransactionScratch(privKey: PrivateKey25519,
                                 timestamp: Long,
                                 useBoxes: Seq[MonetaryBox],
                                 height: Height): EncryTransaction = {

    val pubKey = privKey.publicImage

    val unlockers = useBoxes.map(bx => Unlocker(bx.id, None)).toIndexedSeq

    val directives = if (useBoxes.nonEmpty) {
      IndexedSeq(CoinbaseDirective(height),
        TransferDirective(pubKey.address, useBoxes.map(_.amount).sum, 1))
    } else {
      IndexedSeq(CoinbaseDirective(height))
    }


    val signature = privKey.sign(EncryTransaction.getMessageToSign(pubKey, 0, timestamp, unlockers, directives))

    EncryTransaction(pubKey, 0, timestamp, signature, unlockers, directives)
  }
}
