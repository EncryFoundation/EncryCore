package encry.it.docker

import encry.utils.{NetworkTimeProvider, NetworkTimeProviderSettings}
import org.scalatest.{BeforeAndAfterAll, Suite}
import scala.concurrent.duration._

trait NTPTime extends BeforeAndAfterAll { _: Suite =>
  val ntp: NetworkTimeProviderSettings = NetworkTimeProviderSettings("pool.ntp.org", 1.seconds, 1.seconds)
  protected val ntpTime                = new NetworkTimeProvider(ntp)

  override protected def afterAll(): Unit = {
    super.afterAll()
    //ntpTime.close()
  }
}
