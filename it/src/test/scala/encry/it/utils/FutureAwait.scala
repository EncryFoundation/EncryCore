package encry.it.utils

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

object FutureAwait {

  implicit class FutureAwait[T](future: Future[T]) {
    def await(implicit duration: Duration): T = Await.result(future, duration)
  }

}
