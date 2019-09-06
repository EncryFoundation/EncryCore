package encry.settings

import net.ceedubs.ficus.readers.ValueReader

import org.encryfoundation.common.utils.constants.{Constants, TestNetConstants}

trait ConstantsSettingsReader {

  implicit val constantsSettingsReader: ValueReader[Constants] = (cfg, path) => {

    def getConstants(constantsClass: String): Constants = {
      constantsClass match {
        case "TestConstants" => TestConstants
        case "SlowMiningConstants" => SlowMiningConstants
        case _ => TestNetConstants
      }
    }

    println(s"path: $path")

    val keyPath = path//"encry.node.constantsClass"
    getConstants(
      if (cfg.hasPath(keyPath)) cfg.getString(keyPath) else ""
    )

  }

}