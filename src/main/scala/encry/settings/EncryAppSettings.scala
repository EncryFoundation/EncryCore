package encry.settings

import java.io.File
import com.typesafe.config.ConfigFactory
import encry.utils.NetworkTimeProviderSettings
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

case class EncryAppSettings(directory: String,
                            testing: TestingSettings,
                            node: NodeSettings,
                            wallet: WalletSettings,
                            dataDir: File,
                            kafka: KafkaSettings,
                            network: NetworkSettings,
                            restApi: RESTApiSettings,
                            ntp: NetworkTimeProviderSettings,
                            postgres: PostgresSettings,
                            influxDB: InfluxDBSettings,
                            levelDb: LevelDbSettings)

object EncryAppSettings extends SettingsReaders with NodeSettingsReader {

  val read: EncryAppSettings = ConfigFactory.load("local.conf")
    .withFallback(ConfigFactory.load()).as[EncryAppSettings]("encry")

}

case class TestingSettings(minimalFee: Int, amount: Int, defaultRecipientAddress: String, limitPerEpoch: Int)

case class WalletSettings(password: String, seed: Option[String])

case class LevelDbSettings(enable: Boolean, recoverMode: Boolean, batchSize: Int)

case class KafkaSettings(sendToKafka: Boolean, topicName: String, groupId: String, kafkaBrokers: String)

case class InfluxDBSettings(url: String, login: String, password: String, udpPort: Int)

