package encry.api.templates

import encry.common.KeyPairType
import encry.crypto.PublicKey25519
import encry.modifiers.mempool.EncryTransaction
import encry.modifiers.state.box.proof.Signature25519
import encry.settings.Algos
import scorex.crypto.authds.ADKey
import scorex.crypto.signatures.{PublicKey, Signature}

import scala.util.Try

case class AddPubKeyInfoTransactionTemplate(accPubKey: String,
                                            sig: String,
                                            fee: Long,
                                            change: Long,
                                            timestamp: Long,
                                            useBoxes: IndexedSeq[String],
                                            pubKey: String,
                                            pubKeyProof: String,
                                            pubKeyInfo: String,
                                            pubKeyTypeName: String)
  extends BaseTemplate[EncryTransaction] {

  import encry.modifiers.mempool.TransactionFactory._

  override def origin: Try[EncryTransaction] =
    Try {
      val accountPubKey = PublicKey25519(PublicKey @@ Algos.decode(accPubKey).get)
      val signature = Signature25519(Signature @@ Algos.decode(sig).get)
      val useBxIds = useBoxes.map(id => ADKey @@ Algos.decode(id).get)
      val pubKeyToAdd = PublicKey @@ Algos.decode(pubKey).get
      val pubKeyToAddProof = Signature @@ Algos.decode(pubKeyProof).get
      val pubKeyToAddInfo = Algos.decode(pubKeyInfo).get
      val pubKeyToAddType = KeyPairType.pairTypeByName(pubKeyTypeName).typeId
      addPubKeyInfoTransaction(accountPubKey, signature, fee,
        change, timestamp, useBxIds, pubKeyToAdd, pubKeyToAddProof, pubKeyToAddInfo, pubKeyToAddType)
    }
}
