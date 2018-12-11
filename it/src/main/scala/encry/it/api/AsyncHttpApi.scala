package encry.it.api

import java.io.IOException
import java.net.InetSocketAddress
import java.util.concurrent.TimeoutException

import encry.it.docker.Node
import encry.utils.Logging
import org.asynchttpclient._
import org.asynchttpclient.util.HttpConstants
import encry.it.util.GlobalTimer._

import scala.compat.java8.FutureConverters._
import org.scalatest.{Assertions, Matchers}
import org.asynchttpclient.Dsl.{get => _get, post => _post}

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object AsyncHttpApi extends Logging { // scalastyle:ignore

  protected val client: AsyncHttpClient = new DefaultAsyncHttpClient

  implicit class NodeAsyncHttpApi(n: Node) extends Assertions with Matchers {

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
            case e@(_: IOException | _: TimeoutException) =>
              n.log.debug(s"Failed to execute request '$r' with error: $e")
              timer.schedule(executeRequest, interval)
          }
      }

      executeRequest
    }

    def get(path: String, f: RequestBuilder => RequestBuilder = identity): Future[Response] =
      retrying(f(_get(s"${n.nodeApiEndpoint}$path")).build())

    def post(url: String, port: Int, path: String, f: RequestBuilder => RequestBuilder = identity): Future[Response] =
      retrying(f(
        _post(s"$url:$port$path").setHeader("api_key", "integration-test-rest-api")
      ).build())

    def post(path: String, body: String): Future[Response] =
      post(s"${n.nodeApiEndpoint}", n.nodeApiEndpoint.getPort, path,
        (rb: RequestBuilder) => rb.setHeader("Content-type", "application/json").setBody(body))

    def waitForStartup(): Future[this.type] = get("/info").map(_ => this)

    def connect(addressAndPort: String): Future[Unit] = post("/peers/connect", addressAndPort).map(_ => ())

    def postJson[A: Writes](path: String, body: A): Future[Response] =
      post(path, stringify(toJson(body)))

    def connect(address: InetSocketAddress): Future[Unit] =
      postJson("/peers/connect", ConnectReq(address.getHostName, address.getPort)).map(_ => ())

  }
}
