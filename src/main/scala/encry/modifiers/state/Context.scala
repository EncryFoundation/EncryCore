package encry.modifiers.state

import encry.view.state.EncryStateView
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.modifiers.state.box.EncryBaseBox

case class Context(transaction: Transaction, box: EncryBaseBox, state: EncryStateView)