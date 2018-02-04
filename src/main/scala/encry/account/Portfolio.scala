package encry.account

import encry.modifiers.state.box.EncryBaseBox

case class Portfolio(address: Address, balance: Balance, boxes: Option[Seq[EncryBaseBox]] = None) {

  lazy val isEmpty: Boolean = balance == 0 && boxes.isEmpty
}
