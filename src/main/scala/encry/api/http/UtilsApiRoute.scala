package encry.api.http

import java.security.SecureRandom

import akka.actor.ActorRefFactory
import akka.http.scaladsl.server.Route
import encry.settings.{Algos, RESTApiSettings}
import scorex.crypto.hash.Blake2b256

case class UtilsApiRoute(override val settings: RESTApiSettings)(implicit val context: ActorRefFactory) extends ApiRoute {
  private val SeedSize: Int = 32

  private def seed(length: Int): String = {
    val seed: Array[Byte] = new Array[Byte](length)
    new SecureRandom().nextBytes(seed) //seed mutated here!
    Algos.encode(seed)
  }

  override val route: Route = pathPrefix("utils") {
    seedRoute ~ length ~ hashBlake2b
  }

  def seedRoute: Route = (path("seed") & get) (complete(seed(SeedSize)))

  def length: Route = (path("seed" / IntNumber) & get) (length => complete(seed(length)))

  def hashBlake2b: Route = (path("hash" / "blake2b") & post & entity(as[String])) { message =>
    complete(Algos.encode(Blake2b256(message)))
  }
}