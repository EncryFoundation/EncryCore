package encry.it.util

import io.netty.util.{HashedWheelTimer, Timer}

object GlobalTimer {

  val timer: Timer = new HashedWheelTimer()
  sys.addShutdownHook {
    timer.stop()
  }
}