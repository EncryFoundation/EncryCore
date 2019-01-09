package encry.it.util

import io.netty.util.{HashedWheelTimer, Timer}
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal

object GlobalTimer {

  val timer: Timer = new HashedWheelTimer()
  sys.addShutdownHook {
    timer.stop()
  }

  implicit class TimerExt(val timer: Timer) extends AnyVal {
    def schedule[A](f: => Future[A], delay: FiniteDuration): Future[A] = {
      val p = Promise[A]
      try {
        timer.newTimeout(_ => p.completeWith(f), delay.length, delay.unit)
      } catch {
        case NonFatal(e) => p.failure(e)
      }
      p.future
    }

    def sleep(term: FiniteDuration): Future[Unit] = schedule(Future.successful(()), term)

    def retryUntil[A](f: => Future[A], cond: A => Boolean, retryInterval: FiniteDuration)(implicit ec: ExecutionContext): Future[A] =
      f.flatMap(v => if (cond(v)) Future.successful(v) else schedule(retryUntil(f, cond, retryInterval), retryInterval))
  }
}