package encry.api.templates

import encry.account.{Account, Address}
import encry.crypto.PublicKey25519
import encry.modifiers.mempool.EncryTransaction
import encry.modifiers.state.box.proof.Signature25519
import encry.settings.Algos
import scorex.crypto.authds.ADKey
import scorex.crypto.signatures.{PublicKey, Signature}

import scala.util.Try

case class DefaultPaymentTransactionTemplate(accPubKey: String,
                                             sig: String,
                                             fee: Long,
                                             change: Long,
                                             timestamp: Long,
                                             useBoxes: IndexedSeq[String],
                                             recipient: String,
                                             amount: Long)
  extends BaseTemplate[EncryTransaction] {

  import encry.modifiers.mempool.TransactionFactory._

  override def origin: Try[EncryTransaction] = Try {
    val accountPubKey = PublicKey25519(PublicKey @@ Algos.decode(accPubKey).get)
    val signature = Signature25519(Signature @@ Algos.decode(sig).get)
    val useBxIds = useBoxes.map(id => ADKey @@ Algos.decode(id).get)
    if (!Account.validAddress(Address @@ recipient)) throw new Error("Invalid address")
    defaultPaymentTransaction(accountPubKey, signature, fee, change, timestamp, useBxIds, Address @@ recipient, amount)
  }
}
