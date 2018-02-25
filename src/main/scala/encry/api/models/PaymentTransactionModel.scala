package encry.api.models

import encry.account.Address
import encry.crypto.{PublicKey25519, Signature25519}
import encry.modifiers.mempool.PaymentTransaction
import encry.settings.Algos
import scorex.crypto.authds.ADKey
import scorex.crypto.signatures.{PublicKey, Signature}

import scala.util.Try

case class PaymentTransactionModel(proposition: String,
                                   fee: Long,
                                   timestamp: Long,
                                   signature: String,
                                   useBoxes: IndexedSeq[String],
                                   createBoxes: IndexedSeq[(String, Long)])
  extends BaseModel[PaymentTransaction] {

  override def toBaseObjOpt: Option[PaymentTransaction] =
    Try {
      PaymentTransaction(
        PublicKey25519(PublicKey @@ Algos.decode(proposition).get),
        fee,
        timestamp,
        Signature25519(Signature @@ Algos.decode(signature).get),
        useBoxes.map(id => ADKey @@ Algos.decode(id).get),
        createBoxes.map { case (addr, am) => Address @@ addr -> am }
      )
    }.toOption
}
