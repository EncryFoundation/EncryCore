package encry.api.http.routes

import encry.api.models.EncryTransactionModel
import encry.modifiers.InstanceFactory
import encry.settings.Algos
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.parser._
import org.scalatest.{Matchers, PropSpec}

class TransactionsApiRouteSpec extends PropSpec with Matchers {

  private val tx = InstanceFactory.paymentTransactionValid()

  property("payment tx deserialization in sendTransactionR") {

    // TODO: Fix when json deserializer for transaction will be ready
//    val txWrapped = PaymentTransactionModel(
//      Algos.encode(tx.accountPubKey.pubKeyBytes),
//      tx.fee,
//      tx.timestamp,
//      Algos.encode(tx.signature.signature),
//      tx.useBoxes.map(k => Algos.encode(k)),
//      tx.directives
//    )
//
//    val txSerialized = txWrapped.asJson.noSpaces
//
//    val txDeserialized = decode[PaymentTransactionModel](txSerialized)
//
//    txDeserialized.isRight shouldBe true
//
//    txWrapped.proposition shouldEqual txDeserialized.right.get.proposition
//
//    val baseTxDeserialized = txDeserialized.right.get.toBaseObjOpt.get
//
//    tx.id shouldEqual baseTxDeserialized.id
  }
}
