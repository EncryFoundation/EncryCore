package encry.it.forkResolving

import encry.it.configs.Configs
import encry.it.docker.Docker.defaultConf
import encry.it.docker.{Docker, DockerAfterAll}
import encry.it.util.WaitUtils._
import encry.it.utils.FutureBlockedRun._
import org.scalatest.{FunSuite, Matchers}

import scala.concurrent.duration._

class SyncTwoNodesTest extends FunSuite with Matchers with DockerAfterAll {

  implicit val futureDuration: FiniteDuration = 30 minutes
  val heightSeparation = 10 //blocks

  test("Late node should sync with one node") {

    val miningNodeConfig = Configs.mining(true)
      .withFallback(Configs.offlineGeneration(true))
      .withFallback(Configs.knownPeers(Seq()))
      .withFallback(Configs.networkAddress("0.0.0.0:9001"))
      .withFallback(Docker.defaultConf)

    val node1 = docker
      .startNodeInternal(miningNodeConfig.withFallback(Configs.nodeName("node1")))

    node1.waitForFullHeight(heightSeparation).run

    val node2 = docker
      .startNodeInternal(
        Configs.nodeName("node2")
          .withFallback(Configs.mining(false))
          .withFallback(Configs.knownPeers(Seq((node1.nodeIp, 9001))))
          .withFallback(defaultConf)
      )

    val (bestFullHeaderId1, bestFullHeaderId2) =
      waitForEqualsId(node1.bestFullHeaderId.run, node2.bestFullHeaderId.run)

    bestFullHeaderId2 shouldEqual bestFullHeaderId1
  }

}
