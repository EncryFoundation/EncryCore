package encry.api.http.routes

import io.circe.Decoder.Result
import io.circe.Json
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.scalatest.{Matchers, PropSpec}
import encry.utils.TestEntityGenerator.paymentTransactionValid

class TransactionsApiRouteSpec extends PropSpec with Matchers {

  private val tx = paymentTransactionValid

  property("payment tx deserialization in sendTransactionR") {

    val txSerialized: Json = Transaction.jsonEncoder(tx)

    val txDeserialized: Result[Transaction] = Transaction.jsonDecoder.decodeJson(txSerialized)

    txDeserialized.isRight shouldBe true

    tx.id shouldEqual txDeserialized.map(_.id).getOrElse(Array.emptyByteArray)
  }
}
