package encry.api.http

import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.server.{AuthorizationFailedRejection, Directive0, Directives}
import encry.settings.RESTApiSettings
import scorex.crypto.hash.{Blake2b256, Digest}

trait ApiDirectives extends Directives {
  val settings: RESTApiSettings
  val apiKeyHeaderName: String

  lazy val withCors: Directive0 = settings.corsAllowedOrigin.fold(pass) { origin =>
    respondWithHeaders(RawHeader("Access-Control-Allow-Origin", origin))
  }

  lazy val withAuth: Directive0 = optionalHeaderValueByName(apiKeyHeaderName).flatMap {
    case _ if settings.apiKeyHash.isEmpty => pass
    case None => reject(AuthorizationFailedRejection)
    case Some(key) =>
      val keyHash: Digest = Blake2b256(key)
      if (settings.apiKeyHash.exists(_.toCharArray.sameElements(keyHash))) pass
      else reject(AuthorizationFailedRejection)
  }
}