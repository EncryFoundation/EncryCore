package encry.modifiers.state.box

import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.Proposition

// TODO: Should substitute `scorex.core.transaction.box.Box[P]` in the future.
trait EncryBaseBox extends Box[Proposition]
