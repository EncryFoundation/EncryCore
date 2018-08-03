package encry.api.http.routes

import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCodes}
import akka.http.scaladsl.server.{Directive, Directive1, Route}
import encry.ModifierId
import encry.Address
import encry.api.http.ApiRoute
import encry.crypto.encoding.Base58Check
import encry.settings.Algos
import io.circe.Json
import scorex.crypto.authds.ADKey

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.util.Success

trait EncryBaseApiRoute extends ApiRoute {

  implicit val ec: ExecutionContextExecutor = context.dispatcher

  protected def toJsonResponse(js: Json): Route = {
    val resp = complete(HttpEntity(ContentTypes.`application/json`, js.spaces2))
    withCors(resp)
  }

  protected def toJsonResponse(fn: Future[Json]): Route = onSuccess(fn) { toJsonResponse }

  protected def toJsonOptionalResponse(fn: Future[Option[Json]]): Route = {
    onSuccess(fn) {
      case Some(v) => toJsonResponse(v)
      case None => withCors(complete(StatusCodes.NotFound))
    }
  }

  val paging: Directive[(Int, Int)] = parameters("offset".as[Int] ? 0, "limit".as[Int] ? 50)

  val modifierId: Directive1[ModifierId] = pathPrefix(Segment).flatMap { h =>
    Algos.decode(h) match {
      case Success(header) => provide(ModifierId @@ header)
      case _ => reject
    }
  }

  val accountAddress: Directive1[Address] = pathPrefix(Segment).flatMap { addr =>
    Base58Check.decode(addr) match {
      case Success(_) => provide(Address @@ addr)
      case _ => reject
    }
  }

  val boxId: Directive1[ADKey] = pathPrefix(Segment).flatMap { key =>
    Algos.decode(key) match {
      case Success(k) => provide(ADKey @@ k)
      case _ => reject
    }
  }

  implicit class OkJsonResp(fn: Future[Json]) {
    def okJson(): Route = toJsonResponse(fn)
  }

  implicit class OkJsonOptResp(fn: Future[Option[Json]]) {
    def okJson(): Route = toJsonOptionalResponse(fn)
  }
}
