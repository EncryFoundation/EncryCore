package encry.it

import encry.it.configs.Configs
import encry.it.docker.Docker.defaultConf
import encry.it.docker.{DockerAfterAll, Node}
import org.scalatest.{AsyncFunSuite, Matchers}

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class ForkResolvingOnDownloadingTest extends AsyncFunSuite with Matchers with DockerAfterAll {

  implicit class FutureBlockedRun[T](future: Future[T]) {
    def run(implicit duration: Duration): T = Await.result(future, duration)
  }

  implicit val futureDuration: FiniteDuration = 30 minutes
  val heightSeparation = 10 //blocks
  val maxTries: Int = 1800 //seconds

  test("Late node should sync with one node") {

    val miningNodeConfig = Configs.mining(true)
      .withFallback(Configs.offlineGeneration(true))
      .withFallback(Configs.knownPeers(Seq()))
      .withFallback(defaultConf)

    val node1 = dockerSingleton()
      .startNodeInternal(miningNodeConfig.withFallback(Configs.nodeName("node1")))

    node1.waitForFullHeight(heightSeparation).run

    val node2 = dockerSingleton()
      .startNodeInternal(
        Configs.nodeName("node2")
          .withFallback(Configs.mining(false))
          .withFallback(Configs.knownPeers(Seq((node1.nodeIp, 9001))))
          .withFallback(defaultConf)
      )

    val (bestFullHeaderId1, bestFullHeaderId2) =
      waitForEqualsId(node1.bestFullHeaderId.run, node2.bestFullHeaderId.run, maxTries)

    docker.close()

    bestFullHeaderId2 shouldEqual bestFullHeaderId1
  }

  test("Late node should sync with the first of two nodes") {

    val miningNodeConfig = Configs.mining(true)
      .withFallback(Configs.offlineGeneration(true))
      .withFallback(Configs.knownPeers(Seq()))
      .withFallback(defaultConf)

    val node1 = dockerSingleton()
      .startNodeInternal(miningNodeConfig.withFallback(Configs.nodeName("node1")))

    val node2 = dockerSingleton()
      .startNodeInternal(miningNodeConfig.withFallback(Configs.nodeName("node2")))

    node1.waitForFullHeight(heightSeparation).run

    val node3 = dockerSingleton()
      .startNodeInternal(
        Configs.nodeName("node3")
          .withFallback(Configs.mining(false))
          .withFallback(Configs.knownPeers(Seq((node1.nodeIp, 9001), (node2.nodeIp, 9001))))
          .withFallback(defaultConf)
      )

    val (bestFullHeaderId1, bestFullHeaderId3) =
      waitForEqualsId(node1.bestFullHeaderId.run, node3.bestFullHeaderId.run, maxTries)

    docker.close()

    bestFullHeaderId3 shouldEqual bestFullHeaderId1
  }

  test("All nodes should go to first chain") {

    val miningNodeConfig = Configs.mining(true)
      .withFallback(Configs.offlineGeneration(true))
      .withFallback(Configs.knownPeers(Seq()))
      .withFallback(defaultConf)

    val node1 = dockerSingleton()
      .startNodeInternal(miningNodeConfig.withFallback(Configs.nodeName("node1")))

    node1.waitForFullHeight(heightSeparation).run

    val node2 = dockerSingleton()
      .startNodeInternal(miningNodeConfig.withFallback(Configs.nodeName("node2")))

    node2.waitForFullHeight(heightSeparation).run

    val node3 = dockerSingleton()
      .startNodeInternal(
        Configs.nodeName("node3")
          .withFallback(Configs.mining(false))
          .withFallback(Configs.knownPeers(Seq((node1.nodeIp, 9001), (node2.nodeIp, 9001))))
          .withFallback(defaultConf)
      )

    val (bestFullHeaderId13, bestFullHeaderId3) =
      waitForEqualsId(node1.bestFullHeaderId.run, node3.bestFullHeaderId.run, maxTries)

    val (bestFullHeaderId12, bestFullHeaderId2) =
      waitForEqualsId(node1.bestFullHeaderId.run, node2.bestFullHeaderId.run, maxTries)

    docker.close()

    bestFullHeaderId3 shouldEqual bestFullHeaderId13
    bestFullHeaderId2 shouldEqual bestFullHeaderId12
  }

  @tailrec
  final def waitForEqualsId(id1Func: => String, id2Func: => String, maxTries: Int): (String, String) = {
    val id1: String = id1Func
    val id2: String = id2Func
    if (id1 != id2 && maxTries > 0) {
      Thread.sleep(1000)
      waitForEqualsId(id1Func, id2Func, maxTries - 1)
    } else (id1, id2)
  }
}
