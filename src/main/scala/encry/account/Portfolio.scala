package encry.account

import encry.modifiers.state.box.EncryBaseBox
import encry.utils.BalanceCalculator
import io.circe.Encoder
import io.circe.syntax._

case class Portfolio(account: Account,
                     balances: Map[String, Long],
                     boxes: Seq[EncryBaseBox] = Seq.empty) {

  lazy val isEmpty: Boolean = boxes.isEmpty
}

object Portfolio {

  implicit val jsonEncoder: Encoder[Portfolio] = (p: Portfolio) => Map(
    "address" -> p.account.address.toString.asJson,
    "balance" -> p.balances.asJson,
    "boxes" -> p.boxes.map(_.asJson).asJson,
  ).asJson

  def apply(address: Address,
            balance: Map[String, Long],
            boxes: Seq[EncryBaseBox]): Portfolio = new Portfolio(Account(address), balance, boxes)
}
