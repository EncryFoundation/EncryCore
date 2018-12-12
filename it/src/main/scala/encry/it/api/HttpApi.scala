package encry.it.api

import java.io.IOException
import java.util.concurrent.TimeoutException
import encry.utils.Logging
import org.asynchttpclient._
import org.asynchttpclient.util.HttpConstants
import encry.it.util.GlobalTimer._
import io.circe.Encoder
import io.circe.syntax._
import scala.compat.java8.FutureConverters._
import org.asynchttpclient.Dsl.{get => _get, post => _post}
import org.slf4j.{Logger, LoggerFactory}
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

trait HttpApi extends Logging { // scalastyle:ignore

  def restAddress: String

  def nodeRestPort: Int

  val client: AsyncHttpClient

  protected val log: Logger = LoggerFactory.getLogger(s"${getClass.getName} $restAddress")

  def retrying(request: Request,
               interval: FiniteDuration = 1.second,
               statusCode: Int = HttpConstants.ResponseStatusCodes.OK_200): Future[Response] = {
    def executeRequest: Future[Response] = {
      log.info(s"Executing request '$request'")
      client.executeRequest(request, new AsyncCompletionHandler[Response] {
        override def onCompleted(response: Response): Response = {
          if (response.getStatusCode == statusCode) {
            log.info(s"Request: ${request.getUrl} \n Response: ${response.getResponseBody}")
            response
          } else {
            log.info(s"Request:  ${request.getUrl} \n Unexpected status code(${response.getStatusCode}): " +
              s"${response.getResponseBody}")
            throw UnexpectedStatusCodeException(request, response)
          }
        }
      }).toCompletableFuture.toScala
        .recoverWith {
          case e@(_: IOException | _: TimeoutException) =>
            log.info(s"Failed to execute request '$request' with error: ${e.getMessage}")
            timer.schedule(executeRequest, interval)
        }
    }

    executeRequest
  }

  def get(path: String, f: RequestBuilder => RequestBuilder = identity): Future[Response] =
    retrying(f(_get(s"http://$restAddress:$nodeRestPort$path")).build())

  def post(url: String, port: Int, path: String, f: RequestBuilder => RequestBuilder = identity): Future[Response] =
    retrying(f(
      _post(s"$url:$port$path").setHeader("api_key", "integration-test-rest-api")
    ).build())

  def post(path: String, body: String): Future[Response] =
    post(s"http://$restAddress", nodeRestPort, path,
      (rb: RequestBuilder) => rb.setHeader("Content-type", "application/json").setBody(body))

  def waitForStartup: Future[this.type] = get("/info").map(_ => this)

  def connect(addressAndPort: String): Future[Unit] = post("/peers/connect", addressAndPort).map(_ => ())

  def postJson[A: Encoder](path: String, body: A): Future[Response] =
    post(path, body.asJson.toString())

}
