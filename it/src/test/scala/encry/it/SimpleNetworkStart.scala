package encry.it

import com.typesafe.config.ConfigFactory
import encry.it.docker.Docker
import org.scalatest.{Matchers, PropSpec}

class SimpleNetworkStart extends PropSpec with Matchers{

  property("Network start") {

    val docker = Docker()

    docker.network

    val config = ConfigFactory.load("local.conf")
      .withFallback(ConfigFactory.load())

    docker.startNodeInternal(config)
    docker.close()
  }

}
