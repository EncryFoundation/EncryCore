package encry.network

import com.typesafe.config.ConfigFactory
import encry.settings.{EncryAppSettings, NodeSettingsReader, SettingsReaders}
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

object NetworkUtils {

  object TestNetworkSettings extends SettingsReaders with NodeSettingsReader {

    def read(file: String): EncryAppSettings = ConfigFactory.load(file).as[EncryAppSettings]("encry")
  }
}