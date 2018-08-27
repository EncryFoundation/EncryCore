package encry.settings

import java.io.File
import com.typesafe.config.ConfigFactory
import encry.utils.{Logging, NetworkTimeProviderSettings}
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

object EncryAppSettings extends Logging with SettingsReaders with NodeSettingsReader {

  val read: EncryAppSettings = ConfigFactory.load("local.conf")
    .withFallback(ConfigFactory.load()).as[EncryAppSettings]("encry")

}
