package encry.it

import java.nio.file.Paths

import encry.it.configs.Configs
import encry.it.docker.Docker.defaultConf
import encry.it.docker.{DockerAfterAll, Node}
import org.encryfoundation.common.utils.Algos
import org.scalatest.{AsyncFunSuite, Matchers}
import scorex.utils.Random

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class ForkResolvingOnDownloadingTest extends AsyncFunSuite with Matchers with DockerAfterAll {

  implicit class FutureBlockedRun[T](future: Future[T]) {
    def run(implicit duration: Duration): T = Await.result(future, duration)
  }

  implicit val futureDuration: FiniteDuration = 30 minutes
  val heightSeparation = 10 //blocks

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
      waitForEqualsId(node1.bestFullHeaderId.run, node2.bestFullHeaderId.run)

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
      waitForEqualsId(node1.bestFullHeaderId.run, node3.bestFullHeaderId.run)

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
      waitForEqualsId(node1.bestFullHeaderId.run, node3.bestFullHeaderId.run)

    val (bestFullHeaderId12, bestFullHeaderId2) =
      waitForEqualsId(node1.bestFullHeaderId.run, node2.bestFullHeaderId.run)

    docker.close()

    bestFullHeaderId3 shouldEqual bestFullHeaderId13
    bestFullHeaderId2 shouldEqual bestFullHeaderId12
  }

  test("Nodes should sync after restart with new offlineGeneration and port") {

    val node1 = dockerSingleton()
      .startNodeInternal(Configs.mining(true)
        .withFallback(Configs.nodeName("node1"))
        .withFallback(Configs.offlineGeneration(true))
        .withFallback(Configs.knownPeers(Seq()))
        .withFallback(defaultConf)
      )

    val userDir = Paths.get(System.getProperty("user.dir"))
    println(s"userDir: $userDir")

    val volumeName = Algos.encode(Random.randomBytes(32))
    println(s"volumeName: $volumeName")

    val containerMountPath = userDir + "/encry/data"
    println(s"containerMountPath: $containerMountPath")

    val node21 = dockerSingleton()
      .startNodeInternal(Configs.mining(true)
        .withFallback(Configs.nodeName("node21"))
        .withFallback(Configs.offlineGeneration(false))
        .withFallback(Configs.knownPeers(Seq()))
        .withFallback(defaultConf),
        Some(volumeName, containerMountPath)
      )

    node1.connect(s"${node21.nodeIp}:9001").run
    node21.connect(s"${node1.nodeIp}:9001").run

    node1.waitForFullHeight(5).run

    println("node21 try stop")

    node21.shutdown
    Thread.sleep(5000)
    docker.stopNode(node21, 5)
    Thread.sleep(7000)

    println("node21 stopped")

    node1.waitForFullHeight(10).run

    println("node2 start again")

    val node22 = dockerSingleton()
      .startNodeInternal(Configs.mining(true)
        .withFallback(Configs.nodeName("node22"))
        .withFallback(Configs.offlineGeneration(true))
        .withFallback(Configs.knownPeers(Seq()))
        .withFallback(Configs.networkAddress("0.0.0.0:9002"))
        .withFallback(Configs.apiAddress("0.0.0.0:9052"))
        .withFallback(defaultConf),
        Some(volumeName, containerMountPath)
      )

    println("node22 started")

    node1.waitForFullHeight(15).run

    println("connect again")

    node1.connect(s"${node22.nodeIp}:9002").run
    node22.connect(s"${node1.nodeIp}:9001").run

    node1.waitForFullHeight(20).run

    println("try to sync")

    val (bestFullHeaderId1, bestFullHeaderId2) =
      waitForEqualsId(node1.bestFullHeaderId.run, node22.bestFullHeaderId.run)

    bestFullHeaderId2 shouldEqual bestFullHeaderId1
  }

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
