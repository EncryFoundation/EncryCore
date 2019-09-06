package encry.it

import com.typesafe.config.Config
import encry.consensus.EncrySupplyController
import encry.it.configs.Configs
import encry.it.docker.NodesFromDocker
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.Height
import org.scalatest.{AsyncFunSuite, Matchers}
import encry.settings.EncryAppSettings.settings.constants
import scala.concurrent.Await
import scala.concurrent.duration._

class MinerWalletBalanceTest extends AsyncFunSuite with Matchers with NodesFromDocker {

  override protected def nodeConfigs: Seq[Config] = Seq(Configs.mining(true)
    .withFallback(Configs.offlineGeneration(true))
    .withFallback(Configs.nodeName("node1")))

  test("Miner balance should increase ") {

    val heightToCheck = 5
    val supplyAtHeight = (0 to heightToCheck).foldLeft(0: Long) {
      case (supply, i) => supply + EncrySupplyController.supplyAt(Height @@ i)
    }

    val height = dockerNodes().head.waitForHeadersHeight(heightToCheck)
    Await.result(height, 30.minutes)
    height map { _ =>
      val res = Await.result(dockerNodes().head.balances, 30.minutes)
        .find(_._1 == Algos.encode(constants.IntrinsicTokenId))
        .map(_._2 == supplyAtHeight)
        .get
      docker.close()
      res shouldEqual true
    }
  }
}
