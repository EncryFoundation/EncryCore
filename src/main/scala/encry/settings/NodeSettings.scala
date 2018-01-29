package encry.settings

import scala.concurrent.duration.FiniteDuration

case class NodeSettings(ADState: Boolean,
                        verifyTransactions: Boolean,
                        blocksToKeep: Int,
                        mining: Boolean,
                        miningDelay: FiniteDuration,
                        offlineGeneration: Boolean,
                        keepVersions: Int,
                        utxMaxAge: FiniteDuration,
                        mempoolCleanupInterval: FiniteDuration,
                        mempoolMaxCapacity: Int)
