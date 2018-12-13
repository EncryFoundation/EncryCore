package encry.it

import com.typesafe.config.ConfigFactory
import encry.it.docker.Docker
import org.scalatest.{AsyncFunSuite, Matchers}
import scala.concurrent.Await
import scala.concurrent.duration._

class SimpleNetworkStart extends AsyncFunSuite with Matchers {

  test("Wait for headers height = 5") {
    val docker = Docker()
    val config = ConfigFactory.load("local.conf")
      .withFallback(ConfigFactory.load())
    val node = docker.startNodeInternal(config)
    val height = node.waitForHeadersHeight(5)
    Await.result(height, 4.minutes)
    height map { headersHeight =>
      docker.close()
      (headersHeight >= 5) shouldBe true
    }
  }
}
