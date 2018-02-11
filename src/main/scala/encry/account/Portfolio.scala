package encry.account

import encry.modifiers.state.box.EncryBaseBox
import io.circe.Json
import io.circe.syntax._
import scorex.core.serialization.JsonSerializable

case class Portfolio(address: Address, balance: Balance,
                     boxes: Seq[EncryBaseBox] = Seq.empty) extends JsonSerializable {

  lazy val isEmpty: Boolean = balance == 0 && boxes.isEmpty

  override def json: Json = Map(
    "address" -> address.toString.asJson,
    "balance" -> balance.toLong.asJson,
    "boxes" -> boxes.map(_.json).asJson,
  ).asJson
}
