package encry.api.http

import akka.actor.ActorRefFactory
import akka.http.scaladsl.server.{Directive0, Route}
import akka.http.scaladsl.unmarshalling.PredefinedFromEntityUnmarshallers
import akka.util.Timeout
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import encry.utils.ActorHelper

trait ApiRoute extends ApiDirectives with ActorHelper with FailFastCirceSupport with PredefinedFromEntityUnmarshallers {

  def context: ActorRefFactory

  def route: Route

  override val apiKeyHeaderName: String = "api_key"

  //implicit val printer: Printer = Printer.spaces2.copy(dropNullValues = true)
  implicit lazy val timeout: Timeout = Timeout(settings.timeout)

  protected def jsonRoute(fn: ScorexApiResponse, method: Directive0): Route = method {
    withCors(complete(fn))
  }
}