package encry.settings

import java.io.File

import com.typesafe.config.ConfigFactory
import encry.utils.{NetworkTimeProviderSettings, ScorexLogging}
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

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

  val read: EncryAppSettings = ConfigFactory.load("local.conf")
    .withFallback(ConfigFactory.load).as[EncryAppSettings]("encry")

}
