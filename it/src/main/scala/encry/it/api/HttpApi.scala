package encry.it.api

import java.io.IOException
import java.util.concurrent.TimeoutException
import com.typesafe.scalalogging.StrictLogging
import encry.it.util.GlobalTimer._
import io.circe.parser.parse
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import org.asynchttpclient.Dsl.{get => _get, post => _post}
import org.asynchttpclient._
import org.asynchttpclient.util.HttpConstants
import org.slf4j.{Logger, LoggerFactory}
import scala.compat.java8.FutureConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.{FiniteDuration, _}

trait HttpApi { // scalastyle:ignore

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

  def jsonAnswerAs[A](body: String)(implicit d: Decoder[A]): A = parse(body)
    .flatMap(_.as[A])
    .fold(e => throw e, r => r)

  def post(url: String, port: Int, path: String, f: RequestBuilder => RequestBuilder = identity): Future[Response] =
    retrying(f(
      _post(s"$url:$port$path").setHeader("api_key", "integration-test-rest-api")
    ).build())

  def post(path: String, body: String): Future[Response] =
    post(s"http://$restAddress", nodeRestPort, path,
      (rb: RequestBuilder) => rb.setHeader("Content-type", "application/json").setBody(body))

  def fullHeight: Future[Int] = get("/info") flatMap { r =>
    val response = jsonAnswerAs[Json](r.getResponseBody)
    val eitherHeight = response.hcursor.downField("fullHeight").as[Option[Int]]
    eitherHeight.fold[Future[Int]](
      e => Future.failed(new Exception(s"Error getting `fullHeight` from /info response: $e\n$response", e)),
      maybeHeight => Future.successful(maybeHeight.getOrElse(0))
    )
  }

  def headersHeight: Future[Int] = get("/info") flatMap { r =>
    val response = jsonAnswerAs[Json](r.getResponseBody)
    val eitherHeight = response.hcursor.downField("headersHeight").as[Option[Int]]
    eitherHeight.fold[Future[Int]](
      e => Future.failed(new Exception(s"Error getting `headersHeight` from /info response: $e\n$response", e)),
      maybeHeight => Future.successful(maybeHeight.getOrElse(0))
    )
  }

  def balances: Future[Map[String, Long]] = get("/wallet/info") flatMap { r =>
    val response = jsonAnswerAs[Json](r.getResponseBody)
    val eitherBalance = response.hcursor.downField("balances").as[Map[String, String]]
    eitherBalance.fold[Future[Map[String, Long]]](
      e => Future.failed(new Exception(s"Error getting `balances` from /info response: $e\n$response", e)),
      maybeBalance => Future.successful(maybeBalance.map{case (token, balance) => token -> balance.toLong})
    )
  }

  def waitForStartup: Future[this.type] = get("/info").map(_ => this)

  def waitForFullHeight(expectedHeight: Int, retryingInterval: FiniteDuration = 1.second): Future[Int] = {
    waitFor[Int](_.fullHeight, h => h >= expectedHeight, retryingInterval)
  }

  def waitForHeadersHeight(expectedHeight: Int, retryingInterval: FiniteDuration = 1.second): Future[Int] = {
    waitFor[Int](_.headersHeight, h => h >= expectedHeight, retryingInterval)
  }

  def waitFor[A](f: this.type => Future[A], cond: A => Boolean, retryInterval: FiniteDuration): Future[A] = {
    timer.retryUntil(f(this), cond, retryInterval)
  }

  def connect(addressAndPort: String): Future[Unit] = post("/peers/connect", addressAndPort).map(_ => ())

  def postJson[A: Encoder](path: String, body: A): Future[Response] =
    post(path, body.asJson.toString())

}
