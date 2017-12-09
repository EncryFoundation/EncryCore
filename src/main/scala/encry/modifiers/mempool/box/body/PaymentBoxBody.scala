package encry.modifiers.mempool.box

import io.circe.Json
import io.circe.syntax._

case class PaymentBoxBody(amount: Long) extends BaseBoxBody {
  override def json: Json = Map(
    "amount" -> amount.asJson,
  ).asJson
}
