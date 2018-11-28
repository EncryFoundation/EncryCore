package encry.stats

import akka.actor.{Actor, ActorRef}
import com.typesafe.scalalogging.StrictLogging
import encry.stats.LoggingActor.LogMessage
import encry.EncryApp.settings
import encry.stats.KafkaActor.KafkaMessage

class LoggingActor(statSenderOpt: Option[ActorRef] = None, kafkaSenderOpt: Option[ActorRef] = None) extends Actor with StrictLogging {

  override def receive: Receive = {
    case LogMessage(logLevel, logMessage, logsTime) => settings.node.loggingMode match {
      case "Influx" => statSenderOpt.foreach(_ ! LogMessage(logLevel, logMessage, logsTime))
      case "kafka" => kafkaSenderOpt.foreach(_ ! KafkaMessage(logLevel, logMessage))
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