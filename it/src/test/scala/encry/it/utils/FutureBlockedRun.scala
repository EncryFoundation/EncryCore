package encry.it.utils

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

object FutureBlockedRun {

  implicit class FutureBlockedRun[T](future: Future[T]) {
    def run(implicit duration: Duration): T = Await.result(future, duration)
  }

}
