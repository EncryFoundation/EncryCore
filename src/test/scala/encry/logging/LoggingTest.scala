package encry.logging

import org.scalatest.{Matchers, PropSpec}
import org.slf4j.MDC

class LoggingTest extends PropSpec with Matchers{
  import encry.utils.ExtUtils._
  property("mdc") {
//    val reset = "end" -> Console.RESET
//    MDC.put("color", Console.RED)
//    MDC.put("end", Console.RESET)//, reset).foreach(MDC.put())
    "message".logInfo
    "message".logInfo
    "message".logInfo
    "message".logWarn
    "message".logErr
    "message".logInfo
    "message".logInfo
    "message".logInfo
    MDC.clear()
  }

  class Appender extends ch.qos.logback.core.FileAppender {

  }
}
