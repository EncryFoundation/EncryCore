package encry.it

import encry.it.docker.Docker
import org.scalatest.{Matchers, PropSpec}

class SimpleNetworkStart extends PropSpec with Matchers{

  property("Network start") {

    val docker = Docker()

    docker.network
  }

}
