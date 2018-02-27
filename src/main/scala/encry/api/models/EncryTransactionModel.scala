package encry.api.models

import encry.modifiers.mempool.EncryTransaction

import scala.util.Try

case class EncryTransactionModel(proposition: String,
                                 fee: Long,
                                 timestamp: Long,
                                 signature: String,
                                 useBoxes: IndexedSeq[String],
                                 createBoxes: IndexedSeq[(String, Long)])
  extends BaseModel[EncryTransaction] {

  override def toBaseObjOpt: Option[EncryTransaction] =
    Try {
      // TODO: Implement Directive  json serializer.
//      PaymentTransaction(
//        PublicKey25519(PublicKey @@ Algos.decode(proposition).get),
//        fee,
//        timestamp,
//        Signature25519(Signature @@ Algos.decode(signature).get),
//        useBoxes.map(id => ADKey @@ Algos.decode(id).get),
//        createBoxes.map { case (addr, am) => Address @@ addr -> am }
//      )
      throw new Error("Not Implemented")
    }.toOption
}
