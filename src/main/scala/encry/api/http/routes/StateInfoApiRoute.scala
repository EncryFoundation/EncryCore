package encry.api.http.routes

import java.nio.file.Path
import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.model.StatusCodes.InternalServerError
import akka.http.scaladsl.model.{HttpResponse, MediaTypes}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import better.files.File
import better.files._
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import encry.settings.{EncryAppSettings, RESTApiSettings}
import encry.utils.Logging
import encry.view.ReadersHolder.{GetReaders, Readers}
import encry.view.state.{StateMode, UtxoStateReader}
import io.circe.Json
import io.circe.syntax._
import org.encryfoundation.common.utils.TaggedTypes.ADKey
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

case class StateInfoApiRoute(readersHolder: ActorRef,
                             nodeViewActorRef: ActorRef,
                             appSettings: EncryAppSettings,
                             stateMode: StateMode)(implicit val context: ActorRefFactory)
  extends EncryBaseApiRoute with FailFastCirceSupport with Logging {

  override val settings: RESTApiSettings = appSettings.restApi

  override val route: Route = pathPrefix("state") {
    if (settings.enableStateDownload) getBoxByIdR  ~ download
    else getBoxByIdR
  }

  private def getState: Future[UtxoStateReader] = (readersHolder ? GetReaders).mapTo[Readers].map(_.s.get)

  private def getBoxById(id: ADKey): Future[Option[Json]] = getState.map(_.boxById(id).map(_.asJson))

  def getBoxByIdR: Route = (boxId & get) { key =>
    getBoxById(key).okJson()
  }

  def download: Route = (path("download") & get) {
    createZip match {
      case Success(path) => getFromFile(path.toFile, MediaTypes.`application/zip`)
      case Failure(_) => complete(HttpResponse(InternalServerError))
    }
  }

  def createZip: Try[Path] = Try {
    file"${appSettings.directory}/encry.zip".delete(true)
    file"${appSettings.directory}/tmp/".delete(true)
    val dir: File = file"${appSettings.directory}/tmp/".createDirectory()
    val files: Iterator[File] = File(appSettings.directory)
      .list
      .filter(f => Seq("state", "history").exists(f.name.contains(_)))
      .map(_.copyToDirectory(dir))
    logInfo(s"Copying ${files.size} files to tmp directory")
    val zip: File = file"${appSettings.directory}/encry.zip".deleteOnExit()
    dir.zipTo(file"${appSettings.directory}/encry.zip").deleteOnExit()
    zip.path
  }

}
