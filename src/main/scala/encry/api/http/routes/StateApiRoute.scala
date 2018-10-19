package encry.api.http.routes

import java.nio.file.Paths
import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.ContentType.Binary
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, MediaTypes}
import akka.http.scaladsl.server.{RequestContext, Route, RouteResult}
import akka.http.scaladsl.server.directives.{BasicDirectives, ContentTypeResolver}
import akka.stream.ActorAttributes
import akka.stream.scaladsl.FileIO
import encry.settings.{EncryAppSettings, RESTApiSettings}
import encry.utils.Logging
import better.files._
import better.files.File
import scala.concurrent.{Future, Promise}
import scala.util.control.NonFatal

case class StateApiRoute(appSettings: EncryAppSettings)(implicit val context: ActorRefFactory)
  extends EncryBaseApiRoute with Logging with BasicDirectives {
  override val settings: RESTApiSettings = appSettings.restApi

  def copyAndZip(ctx: RequestContext, p: Promise[RouteResult]): Unit = {
    for {
      tempDir <- File.temporaryDirectory(parent = Some(Paths.get(appSettings.directory)))
      _ <- File(appSettings.directory)
        .list
        .filter(f => Seq("state", "history").exists(f.name.contains(_)))
        .map(_.copyToDirectory(tempDir))
    } {
      val zip = tempDir.zipTo(file"${appSettings.directory}/encry.zip")
      p.completeWith {
        getFromDirectory(zip.toJava.getName).apply(ctx)
      }
    }
  }

  override def route: Route = (path("downloadState") & pathEndOrSingleSlash & get) { ctx =>
    val p = Promise[RouteResult]
    copyAndZip(ctx, p)
    p.future.recoverWith {
      case NonFatal(th) =>
        th.printStackTrace()
        Future.failed(th)
    }
  }
}
