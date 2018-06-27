package encry.api.http.routes

import encry.modifiers.InstanceFactory
import encry.modifiers.mempool.EncryTransaction
import io.circe.Decoder.Result
import io.circe.Json
import org.scalatest.{Matchers, PropSpec}

class TransactionsApiRouteSpec extends PropSpec with Matchers with InstanceFactory {

  private val tx = paymentTransactionValid

  property("payment tx deserialization in sendTransactionR") {

    val txSerialized: Json = EncryTransaction.jsonEncoder(tx)

    val txDeserialized: Result[EncryTransaction] = EncryTransaction.jsonDecoder.decodeJson(txSerialized)

    txDeserialized.isRight shouldBe true

    tx.id shouldEqual txDeserialized.map(_.id).getOrElse(Array.emptyByteArray)
  }
}
