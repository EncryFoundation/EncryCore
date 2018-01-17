package encry.modifiers.state.box.unlockers

import scorex.core.transaction.box.BoxUnlocker
import scorex.core.transaction.box.proposition.Proposition
import scorex.crypto.authds.ADKey

trait EncryBoxUnlocker[P <: Proposition] extends BoxUnlocker[P] {

  override val closedBoxId: ADKey
}
