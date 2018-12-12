package encry.it

import com.typesafe.config.ConfigFactory
import encry.it.docker.Docker
import org.scalatest.{Matchers, PropSpec}
import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global

class SimpleNetworkStart extends PropSpec with Matchers{

  property("Network start") {

    val docker = Docker()

    val config = ConfigFactory.load("local.conf")
      .withFallback(ConfigFactory.load())

    docker.startNodeInternal(config)
    Thread.sleep(2500)
    docker.waitForStartupBlocking(docker.nodes.asScala.toList)
    docker.close()
  }

}
