package encry.settings

import java.io.File
import com.typesafe.scalalogging.StrictLogging
import com.typesafe.config.{Config, ConfigFactory}
import encry.EncryApp
import encry.utils.NetworkTimeProviderSettings
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

case class EncryAppSettings(directory: String,
                            node: NodeSettings,
                            wallet: Option[WalletSettings],
                            kafka: Option[KafkaSettings],
                            network: NetworkSettings,
                            storage: StorageSettings,
                            restApi: RESTApiSettings,
                            ntp: NetworkTimeProviderSettings,
                            postgres: Option[PostgresSettings],
                            influxDB: Option[InfluxDBSettings],
                            levelDB: LevelDBSettings,
                            monitoringSettings: Option[MonitoringSettings])

object EncryAppSettings extends SettingsReaders with NodeSettingsReader with StrictLogging {

  val configPath: String = "encry.core"

  val read: EncryAppSettings = ConfigFactory.load("local.conf")
    .withFallback(ConfigFactory.load()).as[EncryAppSettings](configPath)

  private def readConfigFromPath(userConfigPath: Option[String]): Config = {
    val maybeConfigFile = for {
      maybeFilename <- userConfigPath
      file = new File(maybeFilename)
      if file.exists
    } yield file

    maybeConfigFile match {
      // if no user config is supplied, the library will handle overrides/application/reference automatically
      case None =>
        logger.warn("NO CONFIGURATION FILE WAS PROVIDED. STARTING WITH DEFAULT SETTINGS FOR TESTNET!")
        ConfigFactory.load("local.conf")
          .withFallback(ConfigFactory.load())
      // application config needs to be resolved wrt both system properties *and* user-supplied config.
      case Some(file) =>
        val cfg = ConfigFactory.parseFile(file)
        if (!cfg.hasPath("encry")) failWithError("`encry` path missed")
        ConfigFactory
          .defaultOverrides()
          .withFallback(cfg)
          .withFallback(ConfigFactory.defaultApplication())
          .withFallback(ConfigFactory.defaultReference())
          .resolve()
    }
  }

  def read(userConfigPath: Option[String]): EncryAppSettings = {
    fromConfig(readConfigFromPath(userConfigPath))
  }

  val allConfig: Config = ConfigFactory.load("local.conf")
    .withFallback(ConfigFactory.load())
  def fromConfig(config: Config): EncryAppSettings = {

    val directory = config.as[String](s"$configPath.directory")
    val nodeSettings = config.as[NodeSettings](s"$configPath.node")
    val walletSettings = config.as[Option[WalletSettings]](s"$configPath.wallet")
    val kafkaSettings = config.as[Option[KafkaSettings]](s"$configPath.kafka")
    val networkSettings = config.as[NetworkSettings](s"$configPath.network")
    val restApiSettings = config.as[RESTApiSettings](s"$configPath.restApi")
    val storageSettings = config.as[StorageSettings](s"$configPath.storage")
    val ntpSettings = config.as[NetworkTimeProviderSettings](s"$configPath.ntp")
    val postgresSettings = config.as[Option[PostgresSettings]](s"$configPath.postgres")
    val influxSettings = config.as[Option[InfluxDBSettings]](s"$configPath.influxDB")
    val levelDb = config.as[LevelDBSettings](s"$configPath.levelDB")
    val monitoringSettings = config.as[Option[MonitoringSettings]](s"$configPath.monitoringSettings")

    EncryAppSettings(
      directory,
      nodeSettings,
      walletSettings,
      kafkaSettings,
      networkSettings,
      storageSettings,
      restApiSettings,
      ntpSettings,
      postgresSettings,
      influxSettings,
      levelDb,
      monitoringSettings
    )
  }

  private def failWithError(msg: String): Nothing = {
    logger.error(s"Stop application due to malformed configuration file: $msg")
    EncryApp.forceStopApplication()
  }
}

case class WalletSettings(password: String, seed: Option[String])

case class KafkaSettings(sendToKafka: Boolean, topicName: String, groupId: String, kafkaBrokers: String)

case class InfluxDBSettings(url: String, login: String, password: String, udpPort: Int)

