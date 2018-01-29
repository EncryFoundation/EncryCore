package encry.api.http.routes

import encry.api.models.PaymentTransactionModel
import encry.modifiers.mempool.InstanceFactory
import encry.settings.Algos
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.parser._
import org.scalatest.{Matchers, PropSpec}

class TransactionsApiRouteSpec extends PropSpec with Matchers {

  private val tx = InstanceFactory.paymentTransactionValid

  property("payment tx deserialization in sendTransactionR") {

    val txWrapped = PaymentTransactionModel(
      Algos.encode(tx.proposition.pubKeyBytes),
      tx.fee,
      tx.timestamp,
      Algos.encode(tx.signature.signature),
      tx.useBoxes.map(k => Algos.encode(k)),
      tx.createBoxes
    )

    val txSerialized = txWrapped.asJson.noSpaces

    val txDeserialized = decode[PaymentTransactionModel](txSerialized)

    txDeserialized.isRight shouldBe true

    txWrapped.proposition shouldEqual txDeserialized.right.get.proposition

    val baseTxDeserialized = txDeserialized.right.get.toBaseObj

    tx.id shouldEqual baseTxDeserialized.id
  }
}
