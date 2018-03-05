package encry.modifiers.mempool

import encry.account.Address
import encry.crypto.{PrivateKey25519, PublicKey25519}
import encry.modifiers.mempool.directive.{AddPubKeyInfoDirective, CoinbaseDirective, TransferDirective}
import encry.modifiers.state.box.AmountCarryingBox
import encry.modifiers.state.box.proof.Signature25519
import encry.view.history.Height
import scorex.core.transaction.box.Box.Amount
import scorex.crypto.authds.ADKey
import scorex.crypto.signatures.{PublicKey, Signature}

object TransactionFactory {

  def defaultPaymentTransactionScratch(privKey: PrivateKey25519,
                                       fee: Amount,
                                       timestamp: Long,
                                       useBoxes: IndexedSeq[AmountCarryingBox],
                                       recipient: Address,
                                       amount: Amount): EncryTransaction = {

    val pubKey = privKey.publicImage

    val unlockers = useBoxes.map(bx => Unlocker(bx.id, None)).toIndexedSeq

    val change = useBoxes.map(_.amount).sum - (amount + fee)

    val directives = if (change > 0) {
      IndexedSeq(TransferDirective(recipient, amount, 0), TransferDirective(pubKey.address, change, 1))
    } else {
      IndexedSeq(TransferDirective(recipient, amount, 0))
    }

    val signature = privKey.sign(EncryTransaction.getMessageToSign(pubKey, fee, timestamp, unlockers, directives))

    EncryTransaction(pubKey, fee, timestamp, signature, unlockers, directives)
  }

  def defaultPaymentTransaction(accPubKey: PublicKey25519,
                                signature: Signature25519,
                                fee: Amount,
                                change: Amount,
                                timestamp: Long,
                                useBoxesIds: IndexedSeq[ADKey],
                                recipient: Address,
                                amount: Amount): EncryTransaction = {

    val unlockers = useBoxesIds.map(id => Unlocker(id, None)).toIndexedSeq

    val directives = if (change > 0) {
      IndexedSeq(TransferDirective(recipient, amount, 0), TransferDirective(accPubKey.address, change, 1))
    } else {
      IndexedSeq(TransferDirective(recipient, amount, 0))
    }

    EncryTransaction(accPubKey, fee, timestamp, signature, unlockers, directives)
  }

  def coinbaseTransactionScratch(privKey: PrivateKey25519,
                                 timestamp: Long,
                                 useBoxes: Seq[AmountCarryingBox],
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

  def addPubKeyInfoTransactionScratch(privKey: PrivateKey25519,
                                      fee: Amount,
                                      timestamp: Long,
                                      useBoxes: IndexedSeq[AmountCarryingBox],
                                      pubKeyBytes: PublicKey,
                                      pubKeyProofBytes: Signature,
                                      pubKeyInfoBytes: Array[Byte],
                                      pubKeyTypeId: Byte): EncryTransaction = {

    val pubKey = privKey.publicImage

    val unlockers = useBoxes.map(bx => Unlocker(bx.id, None)).toIndexedSeq

    val change = useBoxes.map(_.amount).sum - fee

    val directives = if (change > 0) {
      IndexedSeq(
        AddPubKeyInfoDirective(pubKey.address, pubKeyBytes, pubKeyProofBytes, pubKeyInfoBytes, pubKeyTypeId, 0),
        TransferDirective(pubKey.address, change, 1))
    } else {
      IndexedSeq(
        AddPubKeyInfoDirective(pubKey.address, pubKeyBytes, pubKeyProofBytes, pubKeyInfoBytes, pubKeyTypeId, 0))
    }

    val signature = privKey.sign(EncryTransaction.getMessageToSign(pubKey, fee, timestamp, unlockers, directives))

    EncryTransaction(pubKey, fee, timestamp, signature, unlockers, directives)
  }

  def addPubKeyInfoTransaction(accPubKey: PublicKey25519,
                               signature: Signature25519,
                               fee: Amount,
                               change: Amount,
                               timestamp: Long,
                               useBoxesIds: IndexedSeq[ADKey],
                               pubKeyBytes: PublicKey,
                               pubKeyProofBytes: Signature,
                               pubKeyInfoBytes: Array[Byte],
                               pubKeyTypeId: Byte): EncryTransaction = {

    val unlockers = useBoxesIds.map(id => Unlocker(id, None)).toIndexedSeq

    val directives = if (change > 0) {
      IndexedSeq(
        AddPubKeyInfoDirective(accPubKey.address, pubKeyBytes, pubKeyProofBytes, pubKeyInfoBytes, pubKeyTypeId, 0),
        TransferDirective(accPubKey.address, change, 1))
    } else {
      IndexedSeq(
        AddPubKeyInfoDirective(accPubKey.address, pubKeyBytes, pubKeyProofBytes, pubKeyInfoBytes, pubKeyTypeId, 0))
    }

    EncryTransaction(accPubKey, fee, timestamp, signature, unlockers, directives)
  }

}
