package encry.settings

import com.typesafe.config.{Config, ConfigFactory}
import encry.utils.NetworkTimeProviderSettings
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

case class EncryAppSettings(directory: String,
                            node: NodeSettings,
                            wallet: Option[WalletSettings],
                            kafka: Option[KafkaSettings],
                            network: NetworkSettings,
                            restApi: RESTApiSettings,
                            ntp: NetworkTimeProviderSettings,
                            postgres: Option[PostgresSettings],
                            influxDB: Option[InfluxDBSettings],
                            levelDb: Option[LevelDbSettings],
                            monitoringSettings: Option[MonitoringSettings])

object EncryAppSettings extends SettingsReaders with NodeSettingsReader {

  val configPath: String = "encry"

  val read: EncryAppSettings = ConfigFactory.load("local.conf")
    .withFallback(ConfigFactory.load()).as[EncryAppSettings](configPath)

  def fromConfig(config: Config): EncryAppSettings = {

    val directory = config.as[String](s"$configPath.directory")
    val nodeSettings = config.as[NodeSettings](s"$configPath.node")
    val walletSettings = config.as[Option[WalletSettings]](s"$configPath.wallet")
    val kafkaSettings = config.as[Option[KafkaSettings]](s"$configPath.kafka")
    val networkSettings = config.as[NetworkSettings](s"$configPath.network")
    val restApiSettings = config.as[RESTApiSettings](s"$configPath.restApi")
    val ntpSettings = config.as[NetworkTimeProviderSettings](s"$configPath.ntp")
    val postgresSettings = config.as[Option[PostgresSettings]](s"$configPath.postgres")
    val influxSettings = config.as[Option[InfluxDBSettings]](s"$configPath.influxDB")
    val levelDBSettings = config.as[Option[LevelDbSettings]](s"$configPath.levelDb")
    val monitoringSettings = config.as[Option[MonitoringSettings]](s"$configPath.monitoringSettings")

    EncryAppSettings(
      directory,
      nodeSettings,
      walletSettings,
      kafkaSettings,
      networkSettings,
      restApiSettings,
      ntpSettings,
      postgresSettings,
      influxSettings,
      levelDBSettings,
      monitoringSettings
    )
  }

  val allConfig: Config = ConfigFactory.load("local.conf")
    .withFallback(ConfigFactory.load())
}

case class WalletSettings(password: String, seed: Option[String])

case class LevelDbSettings(enableSave: Boolean, enableRestore: Boolean, batchSize: Int)

case class KafkaSettings(sendToKafka: Boolean, topicName: String, groupId: String, kafkaBrokers: String)

case class InfluxDBSettings(url: String, login: String, password: String, udpPort: Int)

