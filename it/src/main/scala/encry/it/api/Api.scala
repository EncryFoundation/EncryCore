package encry.it.api

import io.circe.{Decoder, Json}
import org.asynchttpclient.Response
import scala.concurrent.{ExecutionContext, Future}

object Api {
  implicit class ResponseFutureExt(val f: Future[Response]) extends AnyVal {
    def as[A: Decoder](implicit ec: ExecutionContext): Future[A] = f.map(r => Json.fromString(r.getResponseBody).as[A].right.get)(ec)
  }
}
