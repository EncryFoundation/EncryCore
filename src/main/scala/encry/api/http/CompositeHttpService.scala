package encry.api.http

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import encry.api.http.routes.SwaggerRoute
import encry.settings.RESTApiSettings

case class CompositeHttpService(system: ActorSystem,
                                routes: Seq[ApiRoute],
                                settings: RESTApiSettings,
                                swaggerConf: String) {

  implicit val actorSystem: ActorSystem = system

  val redirectToSwagger: Route = redirect("/swagger", StatusCodes.PermanentRedirect)

  @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
  val compositeRoute: Route = routes.map(_.route).reduce(_ ~ _)
//  ~
//    path("swagger") { getFromResource("swagger-ui/index.html") } ~
//    getFromResourceDirectory("swagger-ui") ~
//    //swaggerRoute.routes ~
//    redirectToSwagger
}