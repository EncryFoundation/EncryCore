package encry.utils

import com.typesafe.scalalogging.StrictLogging

object ExtUtils extends StrictLogging {

  implicit class Appliable[T](val obj: T) extends AnyVal {
    def rapply[S](f: T => S ): S = { f(obj)}
    def iapply[S](f: T => S ): T = { f(obj); obj}
  }

  implicit class Loggable[T](val obj: T) extends AnyVal {
    def logInfo: T = { logger.info(obj.toString); obj}
    def logInfo[U]( u: U): T = { logger.info(u.toString); obj}
    def logInfoWith[S](extractor: T => S ): T = { logger.info(extractor(obj).toString); obj}

    def logDebug: T = { logger.debug(obj.toString); obj}
    def logDebug[U]( u: U): T = { logger.debug(u.toString); obj}
    def logDebugWith[S](extractor: T => S ): T = { logger.debug(extractor(obj).toString); obj}

    def logWarn: T = { logger.warn(obj.toString); obj}
    def logWarn[U]( u: U): T = { logger.warn(u.toString); obj}
    def logWarnWith[S](extractor: T => S ): T = { logger.warn(extractor(obj).toString); obj}

    def logErr: T = { logger.error(obj.toString); obj}
    def logErr[U]( u: U): T = { logger.error(u.toString); obj}
    def logErrWith[S](extractor: T => S ): T = { logger.error(extractor(obj).toString); obj}
  }
}
