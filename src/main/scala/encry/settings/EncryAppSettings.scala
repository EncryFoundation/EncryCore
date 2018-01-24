package encry.settings

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.Ficus._
import encry.EncryApp
import scorex.core.settings.{ScorexSettings, SettingsReaders}
import scorex.core.utils.ScorexLogging

case class EncryAppSettings(directory: String,
                            chainSettings: ChainSettings,
                            testingSettings: TestingSettings,
                            nodeSettings: NodeSettings,
                            walletSettings: WalletSettings,
                            keyManagerSettings: KeyManagerSettings,
                            scorexSettings: ScorexSettings)

object EncryAppSettings extends ScorexLogging with SettingsReaders {

  val configPath: String = "encry"
  val scorexConfigPath: String = "scorex"

  def read(userConfigPath: Option[String]): EncryAppSettings = {
    fromConfig(readConfigFromPath(userConfigPath))
  }

  def fromConfig(config: Config): EncryAppSettings = {

    val directory = config.as[String](s"$configPath.directory")
    val nodeSettings = config.as[NodeSettings](s"$configPath.node")
    val chainSettings = config.as[ChainSettings](s"$configPath.chain")
    val testingSettings = config.as[TestingSettings](s"$configPath.testing")
    val walletSettings = config.as[WalletSettings](s"$configPath.wallet")
    val keyManagerSettings = config.as[KeyManagerSettings](s"$configPath.keyManager")
    val scorexSettings = config.as[ScorexSettings](scorexConfigPath)

    EncryAppSettings(
      directory, chainSettings, testingSettings, nodeSettings, walletSettings, keyManagerSettings, scorexSettings)
  }

  private def readConfigFromPath(userConfigPath: Option[String]): Config = {
    val maybeConfigFile = for {
      maybeFilename <- userConfigPath
      file = new File(maybeFilename)
      if file.exists
    } yield file

    val config = maybeConfigFile match {
      // if no user config is supplied, the library will handle overrides/application/reference automatically
      case None =>
        log.warn("No configuration file was provided. Starting with default settings!")
        ConfigFactory.load()
      // application config needs to be resolved wrt both system properties *and* user-supplied config.
      case Some(file) =>
        val cfg = ConfigFactory.parseFile(file)
        if (!cfg.hasPath("encry")) {
          log.error("Malformed configuration file was provided! Aborting!")
          EncryApp.forceStopApplication()
        }
        ConfigFactory
          .defaultOverrides()
          .withFallback(cfg)
          .withFallback(ConfigFactory.defaultApplication())
          .withFallback(ConfigFactory.defaultReference())
          .resolve()
    }

    config
  }
}
