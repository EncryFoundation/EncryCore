package encry.stats

import akka.actor.Actor
import com.typesafe.scalalogging.StrictLogging
import encry.stats.LoggingActor.LogMessage
import encry.EncryApp.settings
import encry.stats.KafkaActor.KafkaMessage

class LoggingActor extends Actor with StrictLogging {

  override def receive: Receive = {
    case LogMessage(logLevel, logMessage, logsTime) =>
      if (settings.logging.loggingMode == "influx" && settings.node.sendStat)
        context.system.actorSelection("user/statsSender") ! LogMessage(logLevel, logMessage, logsTime)
      else if (settings.logging.loggingMode == "kafka" && settings.kafka.sendToKafka)
        context.system.actorSelection("/user/kafkaActor") ! KafkaMessage(logLevel, logMessage)
      else if (settings.logging.loggingMode == "file")
        logLevel match {
          case "Info" => logger.info(logMessage)
          case "Warn" => logger.warn(logMessage)
          case "Error" => logger.error(logMessage)
          case "Debug" => logger.debug(logMessage)
        }
    case _ =>
  }
}

object LoggingActor {

  case class LogMessage(logLevel: String, logMessage: String, logsTime: Long)

}