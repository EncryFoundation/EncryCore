package encry.it.forkResolving

import java.nio.file.Paths

import encry.it.configs.Configs
import encry.it.docker.Docker.defaultConf
import encry.it.docker.DockerAfterAll
import encry.it.util.WaitUtils._
import encry.it.utils.FutureBlockedRun._
import org.encryfoundation.common.utils.Algos
import org.scalatest.{AsyncFunSuite, Matchers}
import scorex.utils.Random

import scala.concurrent.duration._

class SyncAfterNodeRestartTest extends AsyncFunSuite with Matchers with DockerAfterAll {

  implicit val futureDuration: FiniteDuration = 30 minutes

  test("Nodes should sync after restart with new offlineGeneration and port") {
    val node1 = docker
      .startNodeInternal(Configs.mining(true)
        .withFallback(Configs.nodeName("node1"))
        .withFallback(Configs.offlineGeneration(true))
        .withFallback(Configs.knownPeers(Seq()))
        .withFallback(defaultConf)
      )

    val userDir = Paths.get(System.getProperty("user.dir"))
    val volumeName = Algos.encode(Random.randomBytes(32))
    val containerMountPath = userDir + "/encry/data"

    val node21 = docker
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

    node21.shutdown
    Thread.sleep(5000)
    docker.stopNode(node21, 5)
    Thread.sleep(7000)

    node1.waitForFullHeight(10).run

    val node22 = docker
      .startNodeInternal(Configs.mining(true)
        .withFallback(Configs.nodeName("node22"))
        .withFallback(Configs.offlineGeneration(true))
        .withFallback(Configs.knownPeers(Seq()))
        .withFallback(Configs.networkAddress("0.0.0.0:9002"))
        .withFallback(Configs.apiAddress("0.0.0.0:9052"))
        .withFallback(defaultConf),
        Some(volumeName, containerMountPath)
      )

    node1.waitForFullHeight(15).run

    node1.connect(s"${node22.nodeIp}:9002").run
    node22.connect(s"${node1.nodeIp}:9001").run

    node1.waitForFullHeight(20).run

    val (bestFullHeaderId1, bestFullHeaderId2) =
      waitForEqualsId(node1.bestFullHeaderId.run, node22.bestFullHeaderId.run)

    bestFullHeaderId2 shouldEqual bestFullHeaderId1
  }

}
