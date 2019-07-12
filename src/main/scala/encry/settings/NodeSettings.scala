package encry.settings

import com.typesafe.config.ConfigException
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ValueReader
import scala.concurrent.duration.FiniteDuration

case class NodeSettings(blocksToKeep: Int,
                        modifiersCacheSize: Int,
                        mining: Boolean,
                        numberOfMiningWorkers: Int,
                        miningDelay: FiniteDuration,
                        offlineGeneration: Boolean,
                        utxMaxAge: FiniteDuration,
                        mempoolCleanupInterval: FiniteDuration,
                        mempoolMaxCapacity: Int,
                        mempoolTxSendingInterval: FiniteDuration,
                        bloomFilterCleanupInterval: FiniteDuration,
                        mempoolTransactionsThreshold: Int,
                        bloomFilterCapacity: Long,
                        bloomFilterFailureProbability: Double,
                        useCli: Boolean)

trait NodeSettingsReader {

  implicit val nodeSettingsReader: ValueReader[NodeSettings] = { (cfg, path) =>
    NodeSettings(
      cfg.as[Int](s"$path.blocksToKeep"),
      cfg.as[Int](s"$path.modifiersCacheSize"),
      cfg.as[Boolean](s"$path.mining"),
      cfg.as[Int](s"$path.numberOfMiningWorkers"),
      cfg.as[FiniteDuration](s"$path.miningDelay"),
      cfg.as[Boolean](s"$path.offlineGeneration"),
      cfg.as[FiniteDuration](s"$path.utxMaxAge"),
      cfg.as[FiniteDuration](s"$path.mempoolCleanupInterval"),
      cfg.as[Int](s"$path.mempoolMaxCapacity"),
      cfg.as[FiniteDuration](s"$path.mempoolTxSendingInterval"),
      cfg.as[FiniteDuration](s"$path.bloomFilterCleanupInterval"),
      cfg.as[Int](s"$path.mempoolTransactionsThreshold"),
      cfg.as[Long](s"$path.bloomFilterCapacity"),
      cfg.as[Double](s"$path.bloomFilterFailureProbability"),
      cfg.as[Boolean](s"$path.useCli"),
    )
  }
}