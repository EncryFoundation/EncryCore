package encry.view.state.index

import encry.account.Address
import encry.modifiers.state.box.EncryBaseBox
import encry.view.state.Balance

case class Portfolio(address: Address, balance: Balance, boxes: Option[Seq[EncryBaseBox]] = None) {

  lazy val isEmpty: Boolean = balance == 0 && boxes.isEmpty
}
