package encry.api.http

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import encry.settings.RESTApiSettings

case class CompositeHttpService(system: ActorSystem, routes: Seq[ApiRoute], settings: RESTApiSettings, swaggerConf: String)
  extends CorsSupport {

  implicit val actorSystem: ActorSystem = system

  val swaggerService: SwaggerConfigRoute = new SwaggerConfigRoute(swaggerConf: String, settings: RESTApiSettings)

  val redirectToSwagger: Route = {
    redirect("/swagger", StatusCodes.PermanentRedirect)
  }

  @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
  val compositeRoute: Route = routes.map(_.route).reduce(_ ~ _) ~ corsHandler(swaggerService.route) ~
    path("swagger") {
      getFromResource("swagger-ui/index.html")
    } ~ getFromResourceDirectory("swagger-ui") ~ redirectToSwagger

}
