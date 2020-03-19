package encry.settings

import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ValueReader
import scala.concurrent.duration.FiniteDuration

trait NodeSettingsReader {

  implicit val nodeSettingsReader: ValueReader[NodeSettings] = (cfg, path) =>
    NodeSettings(
      cfg.as[Int](s"$path.blocksToKeep"),
      cfg.as[Int](s"$path.modifiersCacheSize"),
      cfg.as[Boolean](s"$path.mining"),
      cfg.as[Int](s"$path.numberOfMiningWorkers"),
      cfg.as[FiniteDuration](s"$path.miningDelay"),
      cfg.as[Boolean](s"$path.offlineGeneration"),
      cfg.as[Boolean](s"$path.useCli"),
      cfg.as[Boolean](s"$path.isTestMod")
    )
}