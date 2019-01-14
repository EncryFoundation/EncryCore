package encry.modifiers.state.box

import encry.modifiers.mempool.Transaction
import encry.view.state.EncryStateView

case class Context(transaction: Transaction, box: EncryBaseBox, state: EncryStateView)
