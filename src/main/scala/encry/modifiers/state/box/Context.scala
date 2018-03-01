package encry.modifiers.state.box

import encry.modifiers.mempool.EncryBaseTransaction
import encry.view.history.Height

case class Context(transaction: EncryBaseTransaction, height: Height)