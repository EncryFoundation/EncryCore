package encry.settings

import com.typesafe.config.{Config, ConfigFactory}
import org.encryfoundation.common.utils.constants.{Constants, TestNetConstants}

object Constants {

  val keyPath = "encry.node.constantsClass"
  lazy val config: Config = ConfigFactory.load()
  lazy val constants: Constants = getConstants(
    if(config.hasPath(keyPath)) config.getString(keyPath) else ""
  )

  def getConstants(constantsClass: String): Constants = {
    constantsClass match {
      case "TestConstants" =>
        println("TestConstants")
        TestConstants
      case _ =>
        println("TestNetConstants")
        TestNetConstants
    }
  }

}