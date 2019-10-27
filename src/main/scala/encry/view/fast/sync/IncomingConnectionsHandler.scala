package encry.view.fast.sync

import com.typesafe.scalalogging.StrictLogging
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.settings.EncryAppSettings
import org.encryfoundation.common.utils.Algos

final case class IncomingConnectionsHandler(liveConnections: Map[ConnectedPeer, (Int, Long)],
                                            handledRequests: Int,
                                            settings: EncryAppSettings) extends StrictLogging {

  def canBeProcessed(processor: SnapshotProcessor, remote: ConnectedPeer, manifestId: Array[Byte]): Boolean = {
    val cond1 = processor.actualManifest.exists(_.manifestId.sameElements(manifestId))
    val cond2 = liveConnections.size < settings.network.maxConnections
    val cond3 = !liveConnections.contains(remote)
    logger.info(s"Conditions $cond1, $cond2, $cond3")
    logger.info(s"Current manifest id ${processor.actualManifest.map(l => Algos.encode(l.manifestId))}")
    cond1 && cond2 && cond3
  }

  def canProcessResponse(remote: ConnectedPeer): Boolean =
    handledRequests <= settings.snapshotSettings.requestsPerTime &&
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
    this.copy(liveConnections = liveConnections.filter(l => System.currentTimeMillis() - l._2._2 < 1.minutes.toMillis),
              handledRequests = 0)
  }

}

object IncomingConnectionsHandler {
  def empty(settings: EncryAppSettings): IncomingConnectionsHandler = IncomingConnectionsHandler(Map.empty, 0, settings)
}
