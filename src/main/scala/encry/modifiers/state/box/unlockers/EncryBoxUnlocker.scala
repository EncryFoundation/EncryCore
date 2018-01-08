package encry.modifiers.state.box.unlockers

import encry.modifiers.state.box.proposition.AddressProposition
import scorex.core.transaction.box.BoxUnlocker
import scorex.core.transaction.box.proposition.Proposition

trait EncryBoxUnlocker[P <: Proposition] extends BoxUnlocker[P] {

  def isValid(addr: AddressProposition, proposition: P, msg: Array[Byte]): Boolean
}
