package encry.settings

import com.typesafe.config.ConfigException
import encry.view.state.StateMode
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ValueReader

import scala.concurrent.duration.FiniteDuration

case class NodeSettings(stateMode: StateMode,
                        verifyTransactions: Boolean,
                        blocksToKeep: Int,
                        mining: Boolean,
                        numberOfMiningWorkers: Int,
                        miningDelay: FiniteDuration,
                        offlineGeneration: Boolean,
                        keepVersions: Int,
                        utxMaxAge: FiniteDuration,
                        mempoolCleanupInterval: FiniteDuration,
                        mempoolMaxCapacity: Int,
                        enableCLI: Boolean,
                        sendStat: Boolean)

trait NodeSettingsReader {

  implicit val nodeSettingsReader: ValueReader[NodeSettings] = { (cfg, path) =>
    val stateModeKey = s"$path.stateMode"
    val stateMode = stateModeFromString(cfg.as[String](stateModeKey), stateModeKey)
    NodeSettings(stateMode,
      cfg.as[Boolean](s"$path.verifyTransactions"),
      cfg.as[Int](s"$path.blocksToKeep"),
      cfg.as[Boolean](s"$path.mining"),
      cfg.as[Int](s"$path.numberOfMiningWorkers"),
      cfg.as[FiniteDuration](s"$path.miningDelay"),
      cfg.as[Boolean](s"$path.offlineGeneration"),
      cfg.as[Int](s"$path.keepVersions"),
      cfg.as[FiniteDuration](s"$path.utxMaxAge"),
      cfg.as[FiniteDuration](s"$path.mempoolCleanupInterval"),
      cfg.as[Int](s"$path.mempoolMaxCapacity"),
      cfg.as[Boolean](s"$path.useCli"),
      cfg.as[Boolean](s"$path.sendStat"))
  }

  def stateModeFromString(modeName: String, path: String): StateMode = {
    StateMode.values.find(_.verboseName == modeName)
      .getOrElse(throw new ConfigException.BadValue(path, modeName))
  }
}
