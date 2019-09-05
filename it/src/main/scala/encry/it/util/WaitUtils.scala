package encry.it.util

import scala.annotation.tailrec
import scala.concurrent.duration.Duration

object WaitUtils {

  def waitForEqualsId(id1Func: => String, id2Func: => String)(implicit duration: Duration): (String, String) = {
    @tailrec
    def loop(id1Func: => String, id2Func: => String, maxTries: Long): (String, String) = {
      val id1: String = id1Func
      val id2: String = id2Func
      if (id1 != id2 && maxTries > 0) {
        Thread.sleep(1000)
        loop(id1Func, id2Func, maxTries - 1)
      } else (id1, id2)
    }

    loop(id1Func, id2Func, duration.toSeconds)
  }

}
