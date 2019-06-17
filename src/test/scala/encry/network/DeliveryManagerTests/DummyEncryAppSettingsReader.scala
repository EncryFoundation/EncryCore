package encry.network.DeliveryManagerTests

import com.typesafe.config.ConfigFactory
import encry.settings.{EncryAppSettings, NodeSettingsReader, SettingsReaders}
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

object DummyEncryAppSettingsReader extends SettingsReaders with NodeSettingsReader {

  val configPath: String = "encry"

  val read: EncryAppSettings = ConfigFactory.load("MainTestSettings.conf").as[EncryAppSettings]("encry")

}