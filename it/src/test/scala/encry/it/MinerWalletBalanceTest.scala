package encry.it

import encry.consensus.EncrySupplyController
import encry.it.configs.Configs
import encry.it.docker.Docker
import encry.settings.Constants._
import encry.view.history.History.Height
import org.encryfoundation.common.Algos
import org.scalatest.{AsyncFunSuite, Matchers}
import scala.concurrent.Await
import scala.concurrent.duration._

class MinerWalletBalanceTest extends AsyncFunSuite with Matchers {

  test("Miner balance should increase ") {

    val heightToCheck = 5
    val supplyAtHeight = (0 to heightToCheck).foldLeft(0: Long) {
      case (supply, i) => supply + EncrySupplyController.supplyAt(Height @@ i)
    }

    val docker = Docker()
    val config = Configs.mining(true)
      .withFallback(Configs.knownPeers(Seq.empty))
      .withFallback(Configs.offlineGeneration(true))
      .withFallback(Configs.nodeName("node1234"))

    val nodes = docker.startNodes(Seq(config))
    val height = nodes.head.waitForHeadersHeight(heightToCheck)
    Await.result(height, 10.minutes)
    height map { _ =>
      val res = Await.result(nodes.head.balances, 10.minutes)
        .find(_._1 == Algos.encode(IntrinsicTokenId))
        .map(_._2 == supplyAtHeight)
        .get
      docker.close()
      res shouldEqual true
    }
  }
}
