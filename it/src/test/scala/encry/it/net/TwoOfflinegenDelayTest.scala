package encry.it.net

import encry.it.configs.Configs
import encry.it.docker.Docker.defaultConf
import encry.it.docker.DockerAfterAll
import encry.it.util.WaitUtils._
import org.scalatest.{FunSuite, Matchers}
import encry.it.utils.FutureBlockedRun._

import scala.concurrent.duration._


class TwoOfflinegenDelayTest extends FunSuite with Matchers with DockerAfterAll {

  implicit val futureDuration: FiniteDuration = 20 minutes
  val heightSeparation = 10 //blocks

  test("Third node should sync with two offgen nodes started with delayed") {

    val miningNodeConfig = Configs.mining(true)
      .withFallback(Configs.offlineGeneration(true))
      .withFallback(Configs.networkAddress("0.0.0.0:9001"))
      .withFallback(Configs.knownPeers(Seq()))
      .withFallback(Configs.constantsClass("SlowMiningConstants"))
      .withFallback(defaultConf)

    val node1 = docker
      .startNodeInternal(miningNodeConfig.withFallback(Configs.nodeName("node1")))

    node1.waitForFullHeight(heightSeparation).run

    val node2 = docker
      .startNodeInternal(miningNodeConfig.withFallback(Configs.nodeName("node2")))

    node1.waitForFullHeight(heightSeparation * 2).run

    val node3 = docker
      .startNodeInternal(
        Configs.nodeName("node3")
          .withFallback(Configs.mining(false))
          .withFallback(Configs.networkAddress("0.0.0.0:9001"))
          .withFallback(Configs.knownPeers(Seq((node1.nodeIp, 9001), (node2.nodeIp, 9001))))
          .withFallback(defaultConf)
      )

    val (bestFullHeaderId13, bestFullHeaderId3) =
      waitForEqualsId(node1.bestFullHeaderId.run, node3.bestFullHeaderId.run)

    val (bestFullHeaderId12, bestFullHeaderId2) =
      waitForEqualsId(node1.bestFullHeaderId.run, node2.bestFullHeaderId.run)

    bestFullHeaderId3 shouldEqual bestFullHeaderId13
    bestFullHeaderId2 shouldEqual bestFullHeaderId12
  }
}
