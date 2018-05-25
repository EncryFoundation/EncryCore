package scorex.core.settings

import java.io.File
import java.net.InetSocketAddress

import com.typesafe.config.{Config, ConfigFactory}
import scorex.core.utils.{ByteStr, NetworkTimeProviderSettings, ScorexLogging}
import net.ceedubs.ficus.Ficus._

import scala.concurrent.duration._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

case class RESTApiSettings(bindAddress: InetSocketAddress,
                           apiKeyHash: Option[String],
                           corsAllowedOrigin: Option[String],
                           timeout: FiniteDuration)

case class NetworkSettings(nodeName: String,
                           addedMaxDelay: Option[FiniteDuration],
                           networkChunkSize: Int,
                           localOnly: Boolean,
                           knownPeers: Seq[InetSocketAddress],
                           bindAddress: InetSocketAddress,
                           maxConnections: Int,
                           connectionTimeout: FiniteDuration,
                           upnpEnabled: Boolean,
                           upnpGatewayTimeout: Option[FiniteDuration],
                           upnpDiscoverTimeout: Option[FiniteDuration],
                           declaredAddress: Option[InetSocketAddress],
                           handshakeTimeout: FiniteDuration,
                           deliveryTimeout: FiniteDuration,
                           maxDeliveryChecks: Int,
                           appVersion: String,
                           agentName: String,
                           maxPacketLen: Int,
                           maxInvObjects: Int,
                           syncInterval: FiniteDuration,
                           syncStatusRefresh: FiniteDuration,
                           syncIntervalStable: FiniteDuration,
                           syncStatusRefreshStable: FiniteDuration,
                           syncTimeout: Option[FiniteDuration],
                           controllerTimeout: Option[FiniteDuration]
                          )

case class WalletSettings(seed: ByteStr,
                          password: String,
                          walletDir: File)

case class ScorexSettings(dataDir: File,
                          logDir: File,
                          network: NetworkSettings,
                          restApi: RESTApiSettings,
                          wallet: WalletSettings,
                          ntp: NetworkTimeProviderSettings
                         )


object ScorexSettings extends ScorexLogging with SettingsReaders {

  protected val configPath: String = "scorex"

  def readConfigFromPath(userConfigPath: Option[String], configPath: String): Config = {

    val maybeConfigFile: Option[File] = userConfigPath.map(filename => new File(filename)).filter(_.exists())
      .orElse(userConfigPath.flatMap(filename => Option(getClass.getClassLoader.getResource(filename))).
        map(r => new File(r.toURI)).filter(_.exists()))

    val config = maybeConfigFile match {
      // if no user config is supplied, the library will handle overrides/application/reference automatically
      case None =>
        log.warn("NO CONFIGURATION FILE WAS PROVIDED. STARTING WITH DEFAULT SETTINGS FOR TESTNET!")
        ConfigFactory.load()
      // application config needs to be resolved wrt both system properties *and* user-supplied config.
      case Some(file) =>
        val cfg = ConfigFactory.parseFile(file)
        if (!cfg.hasPath(configPath)) {
          throw new Error("Malformed configuration file was provided! Aborting!")
        }
        ConfigFactory
          .defaultOverrides()
          .withFallback(cfg)
          .withFallback(ConfigFactory.defaultApplication())
          .resolve()
    }

    config
  }

  def read(userConfigPath: Option[String]): ScorexSettings = {
    fromConfig(readConfigFromPath(userConfigPath, configPath))
  }

  def fromConfig(config: Config): ScorexSettings = {
    config.as[ScorexSettings](configPath)
  }
}
