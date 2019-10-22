package encry.view.fastSync

import java.net.InetSocketAddress

final case class IncomingConnectionsHandler(liveConnections: Map[InetSocketAddress, Int], handledRequests: Int) {

  def addNewConnect(remote: InetSocketAddress, chunksNumber: Int): IncomingConnectionsHandler =
    this.copy(liveConnections.updated(remote, chunksNumber))

  def processRequest(remote: InetSocketAddress): IncomingConnectionsHandler = {
    val currentConnections = liveConnections.getOrElse(remote, 0)
    this.copy(liveConnections.updated(remote, currentConnections - 1), handledRequests + 1)
  }
}

object IncomingConnectionsHandler {
  def empty: IncomingConnectionsHandler = IncomingConnectionsHandler(Map.empty, 0)
}
