package encry.it.forkResolving

import encry.it.configs.Configs
import encry.it.docker.Docker.defaultConf
import encry.it.docker.DockerAfterAll
import encry.it.util.WaitUtils._
import encry.it.utils.FutureBlockedRun._
import org.scalatest.{FunSuite, Matchers}

import scala.concurrent.duration._

class SyncThreeNodesKnowAboutEachOtherTest extends FunSuite with Matchers with DockerAfterAll {

  implicit val futureDuration: FiniteDuration = 30 minutes
  val heightSeparation = 10 //blocks

  test("nodes know about each other should sync") {

    val node1 = docker
      .startNodeInternal(Configs.nodeName("node1")
        .withFallback(Configs.mining(true))
        .withFallback(Configs.offlineGeneration(true))
        .withFallback(Configs.networkAddress("0.0.0.0:9001"))
        .withFallback(Configs.knownPeers(Seq()))
        .withFallback(defaultConf)
      )

    node1.waitForFullHeight(heightSeparation).run
    println("try start node2")
    val node2 = docker
      .startNodeInternal(Configs.nodeName("node2")
        .withFallback(Configs.mining(true))
        .withFallback(Configs.offlineGeneration(false))
        .withFallback(Configs.networkAddress("0.0.0.0:9001"))
        .withFallback(Configs.knownPeers(Seq((node1.nodeIp, 9001))))
        .withFallback(defaultConf)
      )

    node1.connect(s"${node2.nodeIp}:9001").run

    node1.waitForFullHeight(heightSeparation * 2).run

    val node3 = docker
      .startNodeInternal(Configs.nodeName("node3")
        .withFallback(Configs.mining(true))
        .withFallback(Configs.offlineGeneration(false))
        .withFallback(Configs.networkAddress("0.0.0.0:9001"))
        .withFallback(Configs.knownPeers(Seq((node1.nodeIp, 9001), (node2.nodeIp, 9001))))
        .withFallback(defaultConf)
      )

    waitForEqualsId(node1.bestFullHeaderId.run, node3.bestFullHeaderId.run)

    val (bestFullHeaderId2, bestFullHeaderId3) =
      waitForEqualsId(node2.bestFullHeaderId.run, node3.bestFullHeaderId.run)

    bestFullHeaderId2 shouldEqual bestFullHeaderId3
  }
}
