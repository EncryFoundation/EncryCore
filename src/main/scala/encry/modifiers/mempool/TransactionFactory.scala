package encry.modifiers.mempool

import encry.account.Address
import encry.crypto.{PrivateKey25519, PublicKey25519}
import encry.modifiers.mempool.directive.TransferDirective
import encry.modifiers.state.box.AssetBox
import scorex.core.transaction.box.Box.Amount

object TransactionFactory {

  def defaultPaymentTransaction(pubKey: PublicKey25519,
                                privKey: PrivateKey25519,
                                fee: Amount,
                                timestamp: Long,
                                useBoxes: Seq[AssetBox],
                                recipient: Address,
                                amount: Amount): EncryTransaction = {

    val unlockers = useBoxes.map(bx => Unlocker(bx.id, None)).toIndexedSeq

    val change = useBoxes.map(_.amount).sum - (amount + fee)

    val directives = if (change > 0) IndexedSeq(
      TransferDirective(recipient, amount, 1),
      TransferDirective(pubKey.address, change, 2))
      else IndexedSeq(TransferDirective(recipient, amount, 1))

    val signature = privKey.sign(EncryTransaction.getMessageToSign(pubKey, fee, timestamp, unlockers, directives))

    EncryTransaction(pubKey, fee, timestamp, signature, unlockers, directives)
  }

  def defaultPaymentTransactionsFromSecrets(keys: Seq[PrivateKey25519],
                                            fee: Amount,
                                            timestamp: Long,
                                            useBoxes: Seq[AssetBox],
                                            recipient: Address,
                                            amount: Amount): Seq[EncryTransaction] =
    keys.map(k => defaultPaymentTransaction(k.publicImage, k, fee, timestamp, useBoxes, recipient, amount))
}
