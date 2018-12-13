package encry.it

import com.typesafe.config.ConfigFactory
import encry.it.configs.Configs
import encry.it.docker.Docker
import org.scalatest.{AsyncFunSuite, Matchers}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class SimpleNetworkStart extends AsyncFunSuite with Matchers {

  test("sa") {

    val b = Configs.knownPeers(List.empty)

    println(b)

    val a = Future{Some("test")}
    a map { str => str shouldBe Some("test")}
  }

  test("Wait for headers height = 5") {
    val docker = Docker()
    val config = ConfigFactory.load("local.conf")
      .withFallback(ConfigFactory.load())
      .withFallback(Configs.mining(true))
      .withFallback(Configs.knownPeers(List.empty))
      .withFallback(Configs.offlineGeneration(true))
    val node = docker.startNodeInternal(config)
    val height = node.waitForHeadersHeight(5)
    Await.result(height, 4.minutes)
    height map { headersHeight =>
      docker.close()
      (headersHeight >= 5) shouldBe true
    }
  }
}
