package encry.api.models

import encry.common.KeyPairType
import encry.crypto.{PublicKey25519, Signature25519}
import encry.modifiers.mempool.AddPubKeyInfoTransaction
import encry.settings.Algos
import scorex.crypto.authds.ADKey
import scorex.crypto.signatures.{PublicKey, Signature}

import scala.util.Try

case class AddPubKeyInfoTransactionModel(accountPubKey: String,
                                         fee: Long,
                                         timestamp: Long,
                                         signature: String,
                                         useBoxes: IndexedSeq[String],
                                         change: Long,
                                         pubKey: String,
                                         pubKeyProof: String,
                                         pubKeyInfo: String,
                                         pubKeyTypeName: String)
  extends BaseModel[AddPubKeyInfoTransaction] {

  override def toBaseObjOpt: Option[AddPubKeyInfoTransaction] =
    Try {
      AddPubKeyInfoTransaction(
        PublicKey25519(PublicKey @@ Algos.decode(accountPubKey).get),
        fee,
        timestamp,
        Signature25519(Signature @@ Algos.decode(signature).get),
        useBoxes.map(id => ADKey @@ Algos.decode(id).get),
        change,
        PublicKey @@ Algos.decode(pubKey).get,
        Signature @@ Algos.decode(pubKeyProof).get,
        Algos.decode(pubKeyInfo).get,
        KeyPairType.pairTypeByName(pubKeyTypeName).typeId
      )
    }.toOption
}
