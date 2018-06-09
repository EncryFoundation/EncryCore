package encry.settings

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import encry.EncryApp
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import scorex.core.utils.{NetworkTimeProviderSettings, ScorexLogging}

case class EncryAppSettings(directory: String,
                            testing: TestingSettings,
                            node: NodeSettings,
                            keyManager: KeyManagerSettings,
                            dataDir: File,
                            logDir: File,
                            network: NetworkSettings,
                            restApi: RESTApiSettings,
                            wallet: WalletSettings,
                            ntp: NetworkTimeProviderSettings,
                            influxDB: InfluxDBSettings)

object EncryAppSettings extends ScorexLogging with SettingsReaders with NodeSettingsReader {

  def read(userConfigPath: Option[String]): EncryAppSettings = fromConfig(readConfigFromPath(userConfigPath))

  def fromConfig(config: Config): EncryAppSettings = {

    val settings = config.as[EncryAppSettings](s"encry")

    if (settings.node.stateMode.isDigest && settings.node.mining) {
      log.error("Malformed configuration file was provided! Mining is not possible with digest state. Aborting!")
      EncryApp.forceStopApplication()
    }

    settings
  }

  private def readConfigFromPath(userConfigPath: Option[String]): Config = {

    val maybeConfigFile: Option[File] = userConfigPath.map(filename => new File(filename)).filter(_.exists())
      .orElse(userConfigPath.flatMap(filename => Option(getClass.getClassLoader.getResource(filename))).
        map(r => new File(r.toURI)).filter(_.exists()))

    maybeConfigFile match {
      case None =>
        log.warn("No configuration file was provided. Starting with default settings!")
        ConfigFactory.load("local")
          .withFallback(ConfigFactory.defaultApplication())
          .resolve()
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
  }
}
