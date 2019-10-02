package encry.it.net

import encry.it.configs.Configs
import encry.it.docker.Docker.defaultConf
import encry.it.docker.DockerAfterAll
import encry.it.util.WaitUtils._
import encry.it.utils.FutureAwait._
import org.scalatest.{FunSuite, Matchers}

import scala.concurrent.duration._

class TwoNodesСonnectedThroughOneTest extends FunSuite with Matchers with DockerAfterAll {

  implicit val futureDuration: FiniteDuration = 10 minutes
  val heightSeparation = 10 //blocks

  test("nodes connected with first should sync") {

    val node1 = docker
      .startNodeInternal(Configs.nodeName("node1")
        .withFallback(Configs.mining(true))
        .withFallback(Configs.offlineGeneration(true))
        .withFallback(Configs.networkAddress("0.0.0.0:9001"))
        .withFallback(Configs.knownPeers(Seq()))
        .withFallback(defaultConf)
      )

    node1.waitForFullHeight(heightSeparation).await

    val node2 = docker
      .startNodeInternal(Configs.nodeName("node2")
        .withFallback(Configs.mining(true))
        .withFallback(Configs.offlineGeneration(false))
        .withFallback(Configs.networkAddress("0.0.0.0:9001"))
        .withFallback(Configs.knownPeers(Seq()))
        .withFallback(defaultConf)
      )

    node1.waitForFullHeight(heightSeparation * 2).await

    val node3 = docker
      .startNodeInternal(Configs.nodeName("node3")
        .withFallback(Configs.mining(true))
        .withFallback(Configs.offlineGeneration(false))
        .withFallback(Configs.networkAddress("0.0.0.0:9001"))
        .withFallback(Configs.knownPeers(Seq((node1.nodeIp, 9001), (node2.nodeIp, 9001))))
        .withFallback(defaultConf)
      )

    waitForEqualsId(node1.bestFullHeaderId.await, node3.bestFullHeaderId.await)

    val (bestFullHeaderId2, bestFullHeaderId3) =
      waitForEqualsId(node2.bestFullHeaderId.await, node3.bestFullHeaderId.await)

    bestFullHeaderId2 shouldEqual bestFullHeaderId3
  }
}
