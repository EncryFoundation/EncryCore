package encry.it.api

import java.net.InetSocketAddress
import encry.it.docker.Node
import org.asynchttpclient.Dsl.{get => _get, post => _post}
import org.asynchttpclient._
import org.asynchttpclient.util.HttpConstants
import org.specs2.matcher.Matchers
import play.api.libs.json.Json.{stringify, toJson}
import play.api.libs.json._
import scala.compat.java8.FutureConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.Future.traverse
import scala.concurrent.duration._
import scala.util.{Failure, Success}

object AsyncHttpApi { // scalastyle:ignore



}
