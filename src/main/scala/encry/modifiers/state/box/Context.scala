package encry.modifiers.state.box

import encry.modifiers.mempool.EncryBaseTransaction
import encry.view.state.EncryStateView

case class Context(transaction: EncryBaseTransaction, state: EncryStateView)
