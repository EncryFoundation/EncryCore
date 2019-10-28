package encry.api.http.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import encry.EncryApp
import encry.settings.RESTApiSettings

case class NodeApiRoute(restApiSettings: RESTApiSettings)(implicit val context: ActorRefFactory)
  extends EncryBaseApiRoute with FailFastCirceSupport {

  override val route: Route = pathPrefix("node")(shutdownR)

  override val settings: RESTApiSettings = restApiSettings

  def shutdownR: Route = path("shutdown") {
    post {
        complete {
          EncryApp.forceStopApplication(errorMessage = "Stopped by http api")
          StatusCodes.OK
        }
    }
  }
}