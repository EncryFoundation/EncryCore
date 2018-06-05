package encry.settings

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import encry.EncryApp
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import scorex.core.settings.{ScorexSettings, SettingsReaders}
import scorex.core.utils.ScorexLogging

case class EncryAppSettings(directory: String,
                            testingSettings: TestingSettings,
                            nodeSettings: NodeSettings,
                            keyManagerSettings: KeyManagerSettings,
                            scorexSettings: ScorexSettings)

object EncryAppSettings extends ScorexLogging with SettingsReaders with NodeSettingsReader {

  val configPath: String = "encry"
  val scorexConfigPath: String = "scorex"

  def read(userConfigPath: Option[String]): EncryAppSettings = {
    fromConfig(readConfigFromPath(userConfigPath))
  }

  def fromConfig(config: Config): EncryAppSettings = {

    val directory = config.as[String](s"$configPath.directory")
    val nodeSettings = config.as[NodeSettings](s"$configPath.node")
    val testingSettings = config.as[TestingSettings](s"$configPath.testing")
    val keyManagerSettings = config.as[KeyManagerSettings](s"$configPath.keyManager")
    val scorexSettings = config.as[ScorexSettings](scorexConfigPath)

    if (nodeSettings.stateMode.isDigest && nodeSettings.mining) {
      log.error("Malformed configuration file was provided! Mining is not possible with digest state. Aborting!")
      EncryApp.forceStopApplication()
    }

    EncryAppSettings(directory, testingSettings, nodeSettings, keyManagerSettings, scorexSettings)
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
