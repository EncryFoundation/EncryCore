package encry.api.http.routes

import akka.actor.ActorRefFactory
import akka.http.scaladsl.server.Route
import encry.settings.RESTApiSettings
import akka.http.scaladsl.model.StatusCodes

case class ArgonRoute(appSettings: RESTApiSettings)(implicit val context: ActorRefFactory) extends EncryBaseApiRoute {
  override val settings: RESTApiSettings = appSettings


  override def route: Route = pathPrefix("argon"){
    {
//      getFromBrowseableDirectory("src/main/resources/argon")
      getFromResourceDirectory("argon")
    }
  }
}
