package encry.it.docker

import com.typesafe.scalalogging.StrictLogging
import org.scalatest.{Args, Status, Suite, SuiteMixin}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

trait ReportingTestName extends SuiteMixin with StrictLogging {
  th: Suite with Nodes =>

  abstract override protected def runTest(testName: String, args: Args): Status = {
    print(s"Test '$testName' started")
    val r = super.runTest(testName, args)
    print(s"Test `$testName` finished with $r")
    r
  }

  private def print(text: String): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val formatted = s"---------- $text ----------"
    logger.debug(formatted)
    try {
      Await.result(Future.traverse(nodes)(_.printDebugMessage(DebugMessage(formatted))), 10.seconds)
    } catch {
      case _: Throwable => ()
    }
  }
}