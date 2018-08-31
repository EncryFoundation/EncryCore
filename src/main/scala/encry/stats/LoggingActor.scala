package encry.stats

import akka.actor.Actor
import com.typesafe.scalalogging.StrictLogging
import encry.stats.LoggingActor.LogMessage
import encry.EncryApp.settings
import encry.stats.KafkaActor.KafkaMessage

class LoggingActor extends Actor with StrictLogging {

  override def receive: Receive = {
    case LogMessage(logLevel, logMessage, logsTime) => settings.node.loggingMode match {
      case "Influx" => context.system.actorSelection(path = "user/statsSender") ! LogMessage(logLevel, logMessage, logsTime)
      case "kafka" => context.system.actorSelection(path = "/user/kafkaActor") ! KafkaMessage(logLevel, logMessage)
      case "file" => logLevel match {
        case "Info" => logger.info(logMessage)
        case "Warn" => logger.warn(logMessage)
        case "Error" => logger.error(logMessage)
        case "Debug" => logger.debug(logMessage)
      }
    }
    case _ =>
  }
}

object LoggingActor {

  case class LogMessage(logLevel: String, logMessage: String, logsTime: Long)

}