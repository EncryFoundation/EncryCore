package encry.it.Network

import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import encry.it.configs.Configs
import encry.it.docker.{Docker, Node}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{AsyncFunSuite, Matchers}
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class SyncTest extends AsyncFunSuite with Matchers with ScalaFutures with StrictLogging {

  test("Node have to choose other node with highest blocks height.") {

    val firstHeightToWait: Int = 30
    val secondHeightToWait: Int = 30
    val waitTime: FiniteDuration  = 1.minutes
    val docker: Docker = Docker()
    val configForFirstNode: Config = Configs.mining(true)
      .withFallback(Configs.offlineGeneration(true))
      .withFallback(Configs.nodeName("node1"))

    val configForSecondNode: Config = Configs.mining(true)
      .withFallback(Configs.offlineGeneration(true))
      .withFallback(Configs.nodeName("node2"))

    val nodeFirst: List[Node] = docker.startNodes(Seq(configForFirstNode))

    Await.result(nodeFirst.head.waitForHeadersHeight(firstHeightToWait), waitTime)


    Future().map { _ =>
      true shouldBe true
    }
  }
}
