package encry.api.json

import encry.account.Address
import encry.modifiers.mempool.PaymentTransaction
import encry.settings.Algos
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.crypto.authds.ADKey
import scorex.crypto.signatures.{PublicKey, Signature}

case class PaymentTransactionDataWrapper(pkBytesEnc: String,
                                         fee: Long,
                                         timestamp: Long,
                                         sigBytesEnc: String,
                                         useBoxes: IndexedSeq[String],
                                         createBoxes: IndexedSeq[(String, Long)])
  extends DataWrapper[PaymentTransaction] {

  // FIXME: .get
  override def toBaseObj: PaymentTransaction = PaymentTransaction(
    PublicKey25519Proposition(PublicKey @@ Algos.decode(pkBytesEnc).get),
    fee,
    timestamp,
    Signature25519(Signature @@ Algos.decode(sigBytesEnc).get),
    useBoxes.map(id => ADKey @@ Algos.decode(id).get),
    createBoxes.map { case (addr, am) => Address @@ addr -> am }
  )
}
