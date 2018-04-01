package encry.modifiers.state.box

import encry.modifiers.mempool.EncryBaseTransaction
import encry.view.history.Height
import scorex.crypto.authds.ADDigest

case class Context(transaction: EncryBaseTransaction,
                   height: Height,
                   lastBlockTimestamp: Long,
                   stateDigest: ADDigest)
