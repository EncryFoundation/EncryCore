package encry.modifiers.state.box

import encry.contracts.EncryStateView
import encry.modifiers.mempool.EncryBaseTransaction

case class Context(transaction: EncryBaseTransaction, state: EncryStateView)
