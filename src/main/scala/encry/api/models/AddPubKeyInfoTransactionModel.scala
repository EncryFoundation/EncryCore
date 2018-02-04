package encry.api.models

import encry.modifiers.mempool.AddPubKeyInfoTransaction
import encry.settings.Algos
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.crypto.authds.ADKey
import scorex.crypto.signatures.{PublicKey, Signature}

import scala.util.Try

case class AddPubKeyInfoTransactionModel(proposition: String,
                                         fee: Long,
                                         timestamp: Long,
                                         signature: String,
                                         useBoxes: IndexedSeq[String],
                                         pubKey: String,
                                         pubKeyProof: String,
                                         pubKeyInfo: String)
  extends BaseModel[AddPubKeyInfoTransaction] {

  override def toBaseObj: Option[AddPubKeyInfoTransaction] =
    Try {
      AddPubKeyInfoTransaction(
        PublicKey25519Proposition(PublicKey @@ Algos.decode(proposition).get),
        fee,
        timestamp,
        Signature25519(Signature @@ Algos.decode(signature).get),
        useBoxes.map(id => ADKey @@ Algos.decode(id).get),
        PublicKey @@ Algos.decode(pubKey).get,
        Signature @@ Algos.decode(pubKeyProof).get,
        pubKeyInfo
      )
    }.toOption
}
