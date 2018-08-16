package encry.utils

import com.typesafe.scalalogging.{Logger, StrictLogging}
import encry.stats.KafkaAgent

trait Logging extends StrictLogging {
  implicit val log: Logger = logger

  def logInfo(message: String): Unit = {
    KafkaAgent.sendEvent(message)
    log.info(message)
  }

  def logWarn(message: String): Unit = {
    KafkaAgent.sendEvent(message)
    log.warn(message)
  }

  def logError(message: String): Unit = {
    KafkaAgent.sendEvent(message)
    log.error(message)
  }

  def logWarn(message: String, cause: Throwable): Unit = {
    KafkaAgent.sendEvent(message)
    log.warn(message, cause)
  }

  def logError(message: String, cause: Throwable): Unit = {
    KafkaAgent.sendEvent(message)
    log.error(message, cause)
  }
}
