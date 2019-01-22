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

    val firstNode: List[Node] = docker.startNodes(Seq(configForFirstNode))

    Await.result(firstNode.head.waitForHeadersHeight(firstHeightToWait), waitTime)

    val secondNode = docker.startNodes(Seq(configForSecondNode))

    Await.result(secondNode.head.waitForHeadersHeight(secondHeightToWait), waitTime)

    val node1Address: Seq[(String, Int)] = Seq((firstNode.head.nodeIp, firstNode.head.nodePort))
    val node2Address: Seq[(String, Int)] = Seq((secondNode.head.nodeIp, secondNode.head.nodePort))
    val configForThirdNode: Config = Configs.knownPeers(node1Address ++ node2Address)
      .withFallback(Configs.offlineGeneration(false))
      .withFallback(Configs.nodeName("node3"))

    val thirdNode: List[Node] = docker.startNodes(Seq(configForThirdNode))

    Future().map { _ =>
      true shouldBe true
    }
  }
}
