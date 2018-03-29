package encry.account

import encry.modifiers.state.box.EncryBaseBox
import io.circe.Encoder
import io.circe.syntax._

case class Portfolio(account: Account, balance: Balance,
                     boxes: Seq[EncryBaseBox] = Seq.empty) {

  lazy val isEmpty: Boolean = balance == 0 && boxes.isEmpty
}

object Portfolio {

  implicit val jsonEncoder: Encoder[Portfolio] = (p: Portfolio) => Map(
    "address" -> p.account.address.toString.asJson,
    "balance" -> p.balance.toLong.asJson,
    "boxes" -> p.boxes.map(_.asJson).asJson,
  ).asJson

  def apply(address: Address, balance: Balance,
            boxes: Seq[EncryBaseBox]): Portfolio = new Portfolio(Account(address), balance, boxes)
}
