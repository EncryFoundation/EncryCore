package encry.it.forkResolving

import encry.it.configs.Configs
import encry.it.docker.Docker.defaultConf
import encry.it.docker.DockerAfterAll
import encry.it.util.WaitUtils._
import encry.it.utils.FutureBlockedRun._
import org.scalatest.{FunSuite, Matchers}

import scala.concurrent.duration._

class SyncThreeNodesTest extends FunSuite with Matchers with DockerAfterAll {

  implicit val futureDuration: FiniteDuration = 30 minutes
  val heightSeparation = 10 //blocks

  test("Late node should sync with the first of two nodes") {

    val miningNodeConfig = Configs.mining(true)
      .withFallback(Configs.offlineGeneration(true))
      .withFallback(Configs.knownPeers(Seq()))
      .withFallback(defaultConf)

    val node1 = docker
      .startNodeInternal(miningNodeConfig.withFallback(Configs.nodeName("node1")))

    val node2 = docker
      .startNodeInternal(miningNodeConfig.withFallback(Configs.nodeName("node2")))

    node1.waitForFullHeight(heightSeparation).run

    val node3 = docker
      .startNodeInternal(
        Configs.nodeName("node3")
          .withFallback(Configs.mining(false))
          .withFallback(Configs.knownPeers(Seq((node1.nodeIp, 9001), (node2.nodeIp, 9001))))
          .withFallback(defaultConf)
      )

    val (bestFullHeaderId1, bestFullHeaderId3) =
      waitForEqualsId(node1.bestFullHeaderId.run, node3.bestFullHeaderId.run)

    bestFullHeaderId3 shouldEqual bestFullHeaderId1
  }
}
