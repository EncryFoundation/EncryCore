package encry.api.http

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.server.{Directive0, Directives, Route}
import akka.http.scaladsl.unmarshalling.PredefinedFromEntityUnmarshallers
import akka.util.Timeout
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import encry.settings.EncryAppSettings

trait ApiRoute extends Directives with FailFastCirceSupport with PredefinedFromEntityUnmarshallers {

  val settings: EncryAppSettings

  lazy val withCors: Directive0 = settings.restApi.corsAllowedOrigin.fold(pass) { origin =>
    respondWithHeaders(RawHeader("Access-Control-Allow-Origin", origin))
  }

  val apiKeyHeaderName: String = "api_key"

  implicit lazy val timeout: Timeout = Timeout(settings.restApi.timeout)

  def context: ActorRefFactory

  def route: Route

  protected def jsonRoute(fn: ScorexApiResponse, method: Directive0): Route = method {
    withCors(complete(fn))
  }
}