package encry.modifiers.state.box.unlockers

import scorex.core.transaction.box.BoxUnlocker
import scorex.core.transaction.box.proposition.Proposition

trait EncryBoxUnlocker[P <: Proposition] extends BoxUnlocker[P]
