package encry.it

import com.spotify.docker.client.DockerClient
import com.spotify.docker.client.DockerClient.ExecCreateParam
import com.typesafe.config.ConfigFactory
import encry.it.configs.Configs
import encry.it.docker.Docker
import org.scalatest.{AsyncFunSuite, Matchers}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class SimpleNodeStart extends AsyncFunSuite with Matchers {

  test("Mining 5 blocks") {
    val docker = Docker()
    val config = ConfigFactory.load
      .withFallback(Configs.mining(true))
      .withFallback(Configs.knownPeers(List.empty))
      .withFallback(Configs.offlineGeneration(true))
      .withFallback(Configs.nodeName("node1"))
    val node = docker.startNodeInternal(config)
    val height = node.waitForHeadersHeight(5)
    Await.result(height, 4.minutes)
    height map { headersHeight =>
      val balance = Await.result(node.balances, 4.minutes)
      println(balance)
      docker.close()
      (headersHeight >= 5) shouldBe true
    }
  }
}
