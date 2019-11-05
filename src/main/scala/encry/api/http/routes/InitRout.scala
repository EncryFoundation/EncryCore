package encry.api.http.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import encry.settings.{EncryAppSettings, RESTApiSettings}
import io.circe.Json

case class InitRout (starter: ActorRef,  appSettings: EncryAppSettings)(
  implicit val context: ActorRefFactory
) extends EncryBaseApiRoute {

  override val settings: RESTApiSettings = appSettings.restApi

  override def route: Route = (path("info") & get) {
    complete("xui")
  }
}
