package encry.utils

import com.typesafe.scalalogging.StrictLogging
import org.slf4j._

object ExtUtils extends StrictLogging {

  implicit class Appliable[T](val obj: T) extends AnyVal {
    def rapply[S](f: T => S ): S = { f(obj)}
    def iapply[S](f: T => S ): T = { f(obj); obj}
  }

  val warningColor: String = Console.YELLOW
  val errorColor: String = Console.RED


  implicit class Loggable[T](val obj: T) extends AnyVal {

    private def coloredPrint[S](color: String, print: => Unit ): T  = {
      MDC.put("color", color)
      MDC.put("reset", Console.RESET)
      print
      MDC.clear()
      obj
    }

    def logInfo: T = logInfoWith(identity)
    def logInfo[U](u: U): T = logInfoWith(_ => u)
    def logInfoWith[S](extractor: T => S ): T = {logger.info(extractor(obj).toString); obj}

    def logDebug: T = logDebugWith(identity)
    def logDebug[U](u: U): T = logDebugWith(_ => u)
    def logDebugWith[S](extractor: T => S ): T = { logger.debug(extractor(obj).toString); obj}

    def logWarn: T = logWarnWith(identity)
    def logWarn[U](u: U): T = logWarnWith(_ => u)
    def logWarnWith[S](extractor: T => S ): T = coloredPrint(warningColor, logger.warn(extractor(obj).toString))

    def logErr: T = logErrWith(identity)
    def logErr[U](u: U): T = logErrWith(_ => u)
    def logErrWith[S](extractor: T => S ): T = coloredPrint(errorColor, logger.error(extractor(obj).toString))
  }
}
