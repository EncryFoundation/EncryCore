package encry.it.api

import java.io.IOException
import java.net.InetSocketAddress
import java.util.concurrent.TimeoutException

import encry.it.docker.Node
import org.asynchttpclient.Dsl.{get => _get, post => _post}
import org.asynchttpclient._
import org.asynchttpclient.util.HttpConstants
import org.specs2.matcher.Matchers

import scala.compat.java8.FutureConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.Future.traverse
import scala.concurrent.duration._
import scala.util.{Failure, Success}

object AsyncHttpApi { // scalastyle:ignore

  def retrying(r: Request,
               interval: FiniteDuration = 1.second,
               statusCode: Int = HttpConstants.ResponseStatusCodes.OK_200,
               waitForStatus: Boolean = false): Future[Response] = {
    def executeRequest: Future[Response] = {
      n.log.trace(s"Executing request '$r'")
      if (r.getStringData != null) n.log.debug(s"Request's body '${r.getStringData}'")
      n.client
        .executeRequest(
          r,
          new AsyncCompletionHandler[Response] {
            override def onCompleted(response: Response): Response = {
              if (response.getStatusCode == statusCode) {
                n.log.debug(s"Request: ${r.getUrl}\nResponse: ${response.getResponseBody}")
                response
              } else {
                n.log.debug(s"Request: ${r.getUrl}\nUnexpected status code(${response.getStatusCode}): ${response.getResponseBody}")
                throw UnexpectedStatusCodeException(r.getUrl, response.getStatusCode, response.getResponseBody)
              }
            }
          }
        )
        .toCompletableFuture
        .toScala
        .recoverWith {
          case e: UnexpectedStatusCodeException if waitForStatus =>
            n.log.debug(s"Failed to execute request '$r' with error: ${e.getMessage}")
            timer.schedule(executeRequest, interval)
          case e @ (_: IOException | _: TimeoutException) =>
            n.log.debug(s"Failed to execute request '$r' with error: ${e.getMessage}")
            timer.schedule(executeRequest, interval)
        }
    }

    executeRequest
  }

  def get(path: String, f: RequestBuilder => RequestBuilder = identity): Future[Response] =
    retrying(f(_get(s"${n.nodeApiEndpoint}$path")).build())

  //def height: Future[Int] = get("/blocks/height").as[JsValue].map(v => (v \ "height").as[Int])

}
