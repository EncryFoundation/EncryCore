package encry.settings

import java.io.File
import java.net.InetSocketAddress

import com.typesafe.scalalogging.StrictLogging
import com.typesafe.config.{Config, ConfigFactory}
import encry.EncryApp
import encry.storage.VersionalStorage.StorageType
import encry.utils.NetworkTimeProviderSettings
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import org.encryfoundation.common.utils.constants.Constants

import scala.concurrent.duration.FiniteDuration

final case class EncryAppSettings(directory: String,
                                  node: NodeSettings,
                                  mempool: MemoryPoolSettings,
                                  wallet: Option[WalletSettings],
                                  network: NetworkSettings,
                                  storage: StorageSettings,
                                  restApi: RESTApiSettings,
                                  ntp: NetworkTimeProviderSettings,
                                  influxDB: Option[InfluxDBSettings],
                                  levelDB: LevelDBSettings,
                                  monitoringSettings: Option[MonitoringSettings],
                                  blackList: BlackListSettings,
                                  constants: Constants)

object EncryAppSettings extends SettingsReaders with NodeSettingsReader with StrictLogging {

  val configPath: String = "encry"

  def read(args: Array[String]): EncryAppSettings =
    if (Option(args).nonEmpty && args.headOption.nonEmpty)
      fromConfig(readConfigFromPath(args.headOption))
    else
      ConfigFactory.load("local.conf")
        .withFallback(ConfigFactory.load()).as[EncryAppSettings](configPath)

  private def readConfigFromPath(userConfigPath: Option[String]): Config = {
    val maybeConfigFile: Option[File] = for {
      maybeFilename <- userConfigPath
      file = new File(maybeFilename) if file.exists
    } yield file

    maybeConfigFile match {
      // if no user config is supplied, the library will handle overrides/application/reference automatically
      case None =>
        logger.warn("NO CONFIGURATION FILE WAS PROVIDED. STARTING WITH DEFAULT SETTINGS FOR TESTNET!")
        ConfigFactory.load("local.conf").withFallback(ConfigFactory.load())
      // application config needs to be resolved wrt both system properties *and* user-supplied config.
      case Some(file) =>
        val cfg: Config = ConfigFactory.parseFile(file)
        if (!cfg.hasPath("encry")) failWithError("`encry` path missed")
        ConfigFactory
          .defaultOverrides()
          .withFallback(cfg)
          .withFallback(ConfigFactory.defaultApplication())
          .withFallback(ConfigFactory.defaultReference())
          .resolve()
    }
  }

  val allConfig: Config = ConfigFactory.load("local.conf")
    .withFallback(ConfigFactory.load())

  def fromConfig(config: Config): EncryAppSettings = {

    val directory = config.as[String](s"$configPath.directory")
    val nodeSettings = config.as[NodeSettings](s"$configPath.node")
    val mempool = config.as[MemoryPoolSettings](s"$configPath.mempool")
    val walletSettings = config.as[Option[WalletSettings]](s"$configPath.wallet")
    val networkSettings = config.as[NetworkSettings](s"$configPath.network")
    val restApiSettings = config.as[RESTApiSettings](s"$configPath.restApi")
    val storageSettings = config.as[StorageSettings](s"$configPath.storage")
    val ntpSettings = config.as[NetworkTimeProviderSettings](s"$configPath.ntp")
    val influxSettings = config.as[Option[InfluxDBSettings]](s"$configPath.influxDB")
    val levelDb = config.as[LevelDBSettings](s"$configPath.levelDB")
    val monitoringSettings = config.as[Option[MonitoringSettings]](s"$configPath.monitoringSettings")
    val blackList = config.as[BlackListSettings](s"$configPath.blackList")
    val constants = config.as[Constants](s"$configPath.constantsClass")

    EncryAppSettings(
      directory,
      nodeSettings,
      mempool,
      walletSettings,
      networkSettings,
      storageSettings,
      restApiSettings,
      ntpSettings,
      influxSettings,
      levelDb,
      monitoringSettings,
      blackList,
      constants
    )
  }

  private def failWithError(msg: String): Nothing =
    EncryApp.forceStopApplication(errorMessage = s"Stop application due to malformed configuration file: $msg")

}

final case class StorageSettings(history: StorageType, state: StorageType)

final case class WalletSettings(password: String, seed: Option[String])

final case class InfluxDBSettings(url: String, login: String, password: String, udpPort: Int)

final case class BlackListSettings(banTime: FiniteDuration, cleanupTime: FiniteDuration)

final case class LevelDBSettings(maxVersions: Int, versionKeySize: Int = 32)

final case class MonitoringSettings(kamonEnabled: Boolean)

final case class RESTApiSettings(enabled: Option[Boolean],
                                 bindAddress: InetSocketAddress,
                                 corsAllowedOrigin: Option[String],
                                 timeout: FiniteDuration)

final case class NetworkSettings(nodeName: Option[String],
                                 addedMaxDelay: Option[FiniteDuration],
                                 networkChunkSize: Int,
                                 localOnly: Option[Boolean],
                                 knownPeers: Seq[InetSocketAddress],
                                 bindAddress: InetSocketAddress,
                                 maxConnections: Int,
                                 connectionTimeout: FiniteDuration,
                                 declaredAddress: Option[InetSocketAddress],
                                 handshakeTimeout: FiniteDuration,
                                 deliveryTimeout: FiniteDuration,
                                 maxDeliveryChecks: Int,
                                 appVersion: String,
                                 maxInvObjects: Int,
                                 connectOnlyWithKnownPeers: Option[Boolean],
                                 modifierDeliverTimeCheck: FiniteDuration,
                                 syncInterval: FiniteDuration,
                                 syncTimeout: Option[FiniteDuration],
                                 syncPacketLength: Int,
                                 maxNumberOfReConnections: Int)

final case class MemoryPoolSettings(utxMaxAge: FiniteDuration,
                                    cleanupInterval: FiniteDuration,
                                    maxCapacity: Int,
                                    txSendingInterval: FiniteDuration,
                                    transactionsLimit: Int,
                                    bloomFilterCleanupInterval: FiniteDuration,
                                    bloomFilterCapacity: Long,
                                    bloomFilterFailureProbability: Double)

final case class NodeSettings(blocksToKeep: Int,
                              modifiersCacheSize: Int,
                              mining: Boolean,
                              numberOfMiningWorkers: Int,
                              miningDelay: FiniteDuration,
                              offlineGeneration: Boolean,
                              useCli: Boolean)
