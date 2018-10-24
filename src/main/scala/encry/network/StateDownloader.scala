package encry.network

import java.nio.file.Paths
import akka.actor.Actor
import akka.pattern.pipe
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpRequest, HttpResponse, StatusCodes}
import akka.persistence.RecoveryCompleted
import akka.stream.scaladsl.FileIO
import encry.settings.EncryAppSettings
import encry.utils.Logging
import encry.EncryApp._
import better.files._
import better.files.File
import encry.view.EncryNodeViewHolder.ReceivableMessages.ReloadState
import scala.concurrent.duration._
import scala.util.{Failure, Success}

class StateDownloader(settings: EncryAppSettings) extends Actor with Logging {

  override def preStart(): Unit = settings.node.downloadStateFrom.foreach(self ! _)

  override def receive: Receive = {
    case address: String =>
      pipe(Http(context.system).singleRequest(HttpRequest(uri = s"http://$address/state/download"))).to(self)
    case HttpResponse(StatusCodes.OK, _, entity, _) =>
      entity.dataBytes.runWith(FileIO.toPath(Paths.get(s"${settings.directory}/encry.zip"))).onComplete {
        case Success(_) =>
          logInfo(s"Successfully downloaded file from other node")
          unzip()
          context.system.scheduler.scheduleOnce(10 seconds) {
            nodeViewHolder ! ReloadState
            peerManager ! RecoveryCompleted
          }
        case Failure(th) =>
          logWarn(s"Failed to download state from other node with exception: ${th.getCause}")
          context.stop(self)
      }
    case resp @ HttpResponse(code, _, _, _) =>
      logWarn("Request failed, response code: " + code)
      resp.discardEntityBytes()
      peerManager ! RecoveryCompleted
  }

  def unzip(): Unit = {
    def moveAllFiles(from: File, dest: File): Unit = from.list.foreach(_.copyToDirectory(dest))
    val zip: File = file"${settings.directory}/encry.zip"
    logInfo(s"Size of downloaded archive is ${zip.size} bytes")
    val unzipped: File = zip.unzipTo(file"${settings.directory}/encry")
    zip.delete(true)
    file"${settings.directory}/history".delete(true)
    file"${settings.directory}/state".delete(true)
    val unzippedHistoryIndex: File = file"${settings.directory}/encry/history/index"
    val unzippedHistoryObjects: File = file"${settings.directory}/encry/history/objects"
    val unzippedState: File = file"${settings.directory}/encry/state"
    val historyIndexDir: File = file"${settings.directory}/history/index/".createDirectories()
    val historyObjectsDir: File = file"${settings.directory}/history/objects/".createDirectories()
    val stateDir: File = file"${settings.directory}/state/".createDirectories()
    moveAllFiles(unzippedHistoryIndex, historyIndexDir)
    moveAllFiles(unzippedHistoryObjects, historyObjectsDir)
    moveAllFiles(unzippedState, stateDir)
    logInfo("Completed unziping and moving files")
    unzipped.delete(true)
  }

}
