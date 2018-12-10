package encry.it.api

import java.io.IOException
import java.util.concurrent.TimeoutException

import encry.it.docker.Node
import encry.utils.Logging
import org.asynchttpclient._
import org.asynchttpclient.util.HttpConstants
import encry.it.util.GlobalTimer._
import org.scalatest.{Assertions, Matchers}

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._
import scala.concurrent.Future

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
              n.log.debug(s"Failed to execute request '$r' with error: ${e.getMessage}")
              timer.schedule(executeRequest, interval)
          }
      }

      executeRequest
    }
  }

  case class UnexpectedStatusCodeException(request: Request, response: Response)
    extends Exception(s"Request: ${request.getUrl}\n Unexpected status code (${response.getStatusCode}): " +
      s"${response.getResponseBody}")
}
