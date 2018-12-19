package encry.it

import com.typesafe.config.ConfigFactory
import encry.it.configs.Configs
import encry.it.docker.Docker
import org.scalatest.{AsyncFunSuite, Matchers}
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class NodeSyncAfterRestartTest extends AsyncFunSuite with Matchers {

  test("Node should sync after restart") {

    val docker = Docker()

    val firstNodeConfig = Configs.mining(true)
      .withFallback(Configs.offlineGeneration(true))
      .withFallback(Configs.nodeName("node123"))

    val secondNodeConfig = Configs.mining(true)
      .withFallback(Configs.offlineGeneration(false))
      .withFallback(Configs.nodeName("node2"))

    val nodes = docker.startNodes(Seq(firstNodeConfig, secondNodeConfig))

    val height = nodes.head.waitForHeadersHeight(30)

    val result = for {
      firstNodeHeight <- nodes.head.waitForHeadersHeight(30)
      secondNodeHeight <- nodes.last.waitForHeadersHeight(30)
    } yield {
      println(s"firstNodeHeight: $firstNodeHeight, secondNodeHeight: $secondNodeHeight")
    }

    Await.result(result, 4.minutes)

    Future("test") map {
      docker.close()
      _ shouldEqual "test"
    }
  }
}
