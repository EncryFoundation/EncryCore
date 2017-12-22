package encry.settings

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.Ficus._
import encry.EncryApp
import scorex.core.utils.ScorexLogging

case class EncryAppSettings(directory: String,
                            consensusSettings: ConsensusSettings,
                            testingSettings: TestingSettings,
                            nodeSettings: NodeSettings)

object EncryAppSettings extends ScorexLogging {

  val configPath: String = "encry"
  val scorexConfigPath: String = "Scorex"

  def read(userConfigPath: Option[String]): EncryAppSettings = {
    fromConfig(readConfigFromPath(userConfigPath))
  }

  def fromConfig(config: Config): EncryAppSettings = {

    val directory = config.as[String](s"$configPath.directory")

    val nodeSettings = config.as[NodeSettings](s"$configPath.node")
    val consensusSettings = config.as[ConsensusSettings](s"$configPath.chain")
    val testingSettings = config.as[TestingSettings](s"$configPath.testing")

    EncryAppSettings(directory, consensusSettings, testingSettings, nodeSettings)
  }

  private def readConfigFromPath(userConfigPath: Option[String]): Config = {
    val maybeConfigFile = for {
      maybeFilename <- userConfigPath
      file = new File(maybeFilename)
      if file.exists
    } yield file

    val config = maybeConfigFile match {
      // if no user config is supplied, the library will handle overrides/application/reference automatically
      case None =>
        log.warn("APPSETTINGS:NO CONFIGURATION FILE WAS PROVIDED. STARTING WITH DEFAULT SETTINGS FOR TESTNET!")
        //println("FALL HERE!!")
        ConfigFactory.load()
      // application config needs to be resolved wrt both system properties *and* user-supplied config.
      case Some(file) =>
        val cfg = ConfigFactory.parseFile(file)
        if (!cfg.hasPath("App")) {
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
    println("SEND CONFIG")
    config
  }
}
