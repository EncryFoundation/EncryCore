package encry.it

import encry.it.configs.Configs
import encry.it.docker.{DockerAfterAll, Node}
import org.scalatest.{AsyncFunSuite, Matchers}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class ForkResolvingOnDownloadingTest extends AsyncFunSuite with Matchers with DockerAfterAll {

  implicit class FutureBlockedRun[T](future: Future[T]) {
    def run(implicit duration: Duration): T = Await.result(future, duration)
  }
  implicit val futureDuration = 10 minutes

  test("Late node should sync with chain") {

    val node1 = dockerSingleton()
      .startNodes(Seq(
        Configs.nodeName("node1")
          .withFallback(Configs.mining(true))
          .withFallback(Configs.offlineGeneration(true))
      ))
      .head

    val node2 = dockerSingleton()
      .startNodes(Seq(
        Configs.nodeName("node2")
          .withFallback(Configs.mining(true))
          .withFallback(Configs.offlineGeneration(true))
      ))
      .head

    node1.waitForStartup.run
    println(s"node1: ${extractNodeInfo(node1)}")

    node2.waitForStartup.run
    println(s"node2: ${extractNodeInfo(node2)}")

    node1.waitForFullHeight(5).run

    val node3 = dockerSingleton()
      .startNodes(Seq(
        Configs.nodeName("node3")
          .withFallback(Configs.mining(false))
          .withFallback(Configs.knownPeers(Seq((node1.nodeIp, 9001), (node2.nodeIp, 9001))))
      ))
      .head

    node3.waitForStartup.run
    println(s"node3: ${extractNodeInfo(node3)}")

    val hight1 = node1.headersHeight.run
    println(s"headersHeight: $hight1")

    node3.waitForFullHeight(hight1).run

    docker.close()

    node3.fullHeight.run shouldEqual hight1
  }

  def extractNodeInfo(node: Node) = s"node: ${node.name}" +
    s"address ${node.address} " +
    s"networkAddress: ${node.networkAddress} " +
    s"nodeApiEndpoint: ${node.nodeApiEndpoint} " +
    s"nodeIp: ${node.nodeIp} " +
    s"nodePort: ${node.nodePort} "

}
