package encry.utils

import com.typesafe.scalalogging.StrictLogging

trait ScorexLogging extends StrictLogging {
  @inline protected def log = logger
  import ExtUtils._
  def logWarn(s: String): String = s.logWarn
  def logWarn(message: String, cause: Throwable): Unit = log.warn(message, cause)

  def logError(s: String) = s.logErr
}
