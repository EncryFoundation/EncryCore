package encry.it

import com.typesafe.config.ConfigFactory
import encry.it.docker.Docker
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{Matchers, PropSpec}
import scala.collection.JavaConverters._

class SimpleNetworkStart extends PropSpec with Matchers with ScalaFutures {

  property("Network start") {

    val docker = Docker()
    val config = ConfigFactory.load("local.conf")
      .withFallback(ConfigFactory.load())
    docker.startNodeInternal(config)
    docker.waitForStartupBlocking(docker.nodes.asScala.toList)
    docker.close()
  }
}
