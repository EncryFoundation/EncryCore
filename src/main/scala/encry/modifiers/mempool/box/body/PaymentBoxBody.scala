package encry.modifiers.mempool.box.body

import io.circe.Json
import io.circe.syntax._

case class PaymentBoxBody(amount: Long) extends BaseBoxBody {
  override def json: Json = Map(
    "amount" -> amount.asJson,
  ).asJson
}
