package encry.view.fast.sync

import java.net.InetSocketAddress

import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.settings.EncryAppSettings

final case class IncomingConnectionsHandler(liveConnections: Map[ConnectedPeer, (Int, Long)],
                                            handledRequests: Int,
                                            settings: EncryAppSettings) {

  def canBeProcessed(processor: SnapshotProcessor, remote: ConnectedPeer, manifestId: Array[Byte]): Boolean =
    processor.actualManifest.exists(_.manifestId.sameElements(manifestId)) &&
      liveConnections.size < settings.network.maxConnections &&
      !liveConnections.contains(remote)

  def canProcessResponse(remote: ConnectedPeer): Boolean =
    handledRequests != settings.snapshotSettings.requestsPerTime &&
      liveConnections.getOrElse(remote, 0 -> 0L)._1 > 0

  def addNewConnect(remote: ConnectedPeer, chunksNumber: Int): IncomingConnectionsHandler =
    this.copy(liveConnections.updated(remote, chunksNumber -> System.currentTimeMillis()))

  def processRequest(remote: ConnectedPeer): IncomingConnectionsHandler = {
    val (currentRequests: Int, _) = liveConnections.getOrElse(remote, 0 -> 0)
    this.copy(liveConnections.updated(remote, (currentRequests - 1, System.currentTimeMillis())), handledRequests + 1)
  }

  def removeConnection(remove: ConnectedPeer): IncomingConnectionsHandler =
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
