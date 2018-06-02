package encry.settings

import java.io.File
import java.net.InetSocketAddress

import com.typesafe.config.{Config, ConfigFactory}
import encry.EncryApp
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.ValueReader
import scorex.core.utils.{ByteStr, NetworkTimeProviderSettings, ScorexLogging}

case class EncryAppSettings(directory: String,
                            testing: TestingSettings,
                            node: NodeSettings,
                            keyManager: KeyManagerSettings,
                            dataDir: File,
                            logDir: File,
                            network: NetworkSettings,
                            restApi: RESTApiSettings,
                            wallet: WalletSettings,
                            ntp: NetworkTimeProviderSettings)

object EncryAppSettings
  extends ScorexLogging with SettingsReaders with NodeSettingsReader {

  val configPath: String = "encry"

  def read(userConfigPath: Option[String]): EncryAppSettings = {
    fromConfig(readConfigFromPath(userConfigPath))
  }

  def fromConfig(config: Config): EncryAppSettings = {

    val directory = config.as[String](s"$configPath.directory")
    val nodeSettings = config.as[NodeSettings](s"$configPath.node")
    val testingSettings = config.as[TestingSettings](s"$configPath.testing")
    val keyManagerSettings = config.as[KeyManagerSettings](s"$configPath.keyManager")
    val networkSettings = config.as[NetworkSettings](s"$configPath.network")
    val restApiSettings = config.as[RESTApiSettings](s"$configPath.restApi")
    val walletSettings = config.as[WalletSettings](s"$configPath.wallet")
    val ntpSettings = config.as[NetworkTimeProviderSettings](s"$configPath.ntp")
    val dataDir = new File(config.as[String](s"$configPath.dataDir"))
    val logDir = new File(config.as[String](s"$configPath.logDir"))

    if (nodeSettings.stateMode.isDigest && nodeSettings.mining) {
      log.error("Malformed configuration file was provided! Mining is not possible with digest state. Aborting!")
      EncryApp.forceStopApplication()
    }

    EncryAppSettings(
      directory,
      testingSettings,
      nodeSettings,
      keyManagerSettings,
      dataDir,
      logDir,
      networkSettings,
      restApiSettings,
      walletSettings,
      ntpSettings
    )
  }

  private def readConfigFromPath(userConfigPath: Option[String]): Config = {

    val maybeConfigFile: Option[File] = userConfigPath.map(filename => new File(filename)).filter(_.exists())
      .orElse(userConfigPath.flatMap(filename => Option(getClass.getClassLoader.getResource(filename))).
        map(r => new File(r.toURI)).filter(_.exists()))

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
