package encry.utils

import com.typesafe.scalalogging.{Logger, StrictLogging}
import encry.utils.ExtUtils._

trait Logging extends StrictLogging {
  implicit val log: Logger = logger
  def logInfo(s: String): String = s.logInfo
  def logWarn(s: String): String = s.logWarn
  def logError(s: String): String = s.logErr
  def logWarn(message: String, cause: Throwable): Unit = log.warn(message, cause)
  def logError(message: String, cause: Throwable): Unit = log.error(message, cause)
}
