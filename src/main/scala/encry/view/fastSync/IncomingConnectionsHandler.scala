package encry.view.fastSync

import java.net.InetSocketAddress
import encry.settings.EncryAppSettings

final case class IncomingConnectionsHandler(liveConnections: Map[InetSocketAddress, (Int, Long)],
                                            handledRequests: Int,
                                            settings: EncryAppSettings) {

  def canBeProcessed(processor: SnapshotProcessor, remote: InetSocketAddress, manifestId: Array[Byte]): Boolean =
    processor.actualManifest.exists(_.manifestId.sameElements(manifestId)) &&
      liveConnections.size < settings.network.maxConnections &&
      !liveConnections.contains(remote)

  def canProcessResponse(remote: InetSocketAddress): Boolean =
    handledRequests != settings.snapshotSettings.requestsPerTime &&
      liveConnections.getOrElse(remote, 0 -> 0L)._1 > 0

  def addNewConnect(remote: InetSocketAddress, chunksNumber: Int): IncomingConnectionsHandler =
    this.copy(liveConnections.updated(remote, chunksNumber -> System.currentTimeMillis()))

  def processRequest(remote: InetSocketAddress): IncomingConnectionsHandler = {
    val (currentRequests: Int, _) = liveConnections.getOrElse(remote, 0 -> 0)
    this.copy(liveConnections.updated(remote, (currentRequests - 1, System.currentTimeMillis())), handledRequests + 1)
  }

  def removeConnection(remove: InetSocketAddress): IncomingConnectionsHandler =
    this.copy(liveConnections - remove)

  def iterationProcessing: IncomingConnectionsHandler = {
    import scala.concurrent.duration._
    this.copy(liveConnections = liveConnections.filter(l => System.currentTimeMillis() - l._2._2 < 10.minutes.toMillis),
              handledRequests = 0)
  }

}

object IncomingConnectionsHandler {
  def empty(settings: EncryAppSettings): IncomingConnectionsHandler = IncomingConnectionsHandler(Map.empty, 0, settings)
}
