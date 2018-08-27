package encry.utils

import encry.EncryApp.{settings, system}
import encry.stats.LoggingActor.LogMessage
import scala.util.Try

trait Logging {

  def info(logMessage: String): Unit = if (settings.logging.loggingMode != "off" && Try(system.name).isSuccess)
    system.actorSelection("user/loggingActor") ! LogMessage("Info", logMessage, System.currentTimeMillis())

  def debug(logMessage: String): Unit = if (settings.logging.loggingMode != "off" && Try(system.name).isSuccess)
    system.actorSelection("user/loggingActor") ! LogMessage("Debug", logMessage, System.currentTimeMillis())

  def warn(logMessage: String): Unit = if (settings.logging.loggingMode != "off" && Try(system.name).isSuccess)
    system.actorSelection("user/loggingActor") ! LogMessage("Warn", logMessage, System.currentTimeMillis())

  def error(logMessage: String): Unit = if (settings.logging.loggingMode != "off" && Try(system.name).isSuccess)
    system.actorSelection("user/loggingActor") ! LogMessage("Error", logMessage, System.currentTimeMillis())
}
