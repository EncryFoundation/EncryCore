package encry.modifiers.state.box

import encry.modifiers.mempool.BaseTransaction
import encry.view.state.EncryStateView

case class Context(transaction: BaseTransaction, state: EncryStateView)
