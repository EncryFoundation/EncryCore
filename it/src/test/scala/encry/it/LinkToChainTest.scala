package encry.it

import com.typesafe.config.Config
import encry.consensus.EncrySupplyController
import encry.it.configs.Configs
import encry.it.docker.NodesFromDocker
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.Height
import org.encryfoundation.common.utils.constants.TestNetConstants
import org.scalatest.{AsyncFunSuite, Matchers}

import scala.concurrent.Await
import scala.concurrent.duration._

class LinkToChainTest extends AsyncFunSuite with Matchers with NodesFromDocker {

  override protected def nodeConfigs: Seq[Config] = Seq(Configs.mining(true)
    .withFallback(Configs.offlineGeneration(true))
    .withFallback(Configs.nodeName("node1")))

  test("Late node should connect to highest chain") {

  }
}
