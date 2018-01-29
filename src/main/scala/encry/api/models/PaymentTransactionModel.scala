package encry.api.models

import encry.account.Address
import encry.modifiers.mempool.PaymentTransaction
import encry.settings.Algos
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.crypto.authds.ADKey
import scorex.crypto.signatures.{PublicKey, Signature}

case class PaymentTransactionModel(proposition: String,
                                   fee: Long,
                                   timestamp: Long,
                                   signature: String,
                                   useBoxes: IndexedSeq[String],
                                   createBoxes: IndexedSeq[(String, Long)])
  extends BaseModel[PaymentTransaction] {

  // FIXME: .get
  override def toBaseObj: PaymentTransaction = PaymentTransaction(
    PublicKey25519Proposition(PublicKey @@ Algos.decode(proposition).get),
    fee,
    timestamp,
    Signature25519(Signature @@ Algos.decode(signature).get),
    useBoxes.map(id => ADKey @@ Algos.decode(id).get),
    createBoxes.map { case (addr, am) => Address @@ addr -> am }
  )
}
