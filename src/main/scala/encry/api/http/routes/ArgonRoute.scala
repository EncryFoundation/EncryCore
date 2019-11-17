package encry.api.http.routes

import akka.actor.ActorRefFactory
import akka.http.scaladsl.server.Route
import encry.settings.RESTApiSettings

case class ArgonRoute(settings: RESTApiSettings)(implicit val context: ActorRefFactory) extends EncryBaseApiRoute {

  override def route: Route = pathPrefix("argon") {
    getFromResourceDirectory("argon")
  }
}
