package encry.network.peer

import java.net.InetSocketAddress
import encry.network.PeerConnectionHandler._

case object peerDatabase {

  private var whitelistPersistence: Map[InetSocketAddress, PeerInfo] = Map[InetSocketAddress, PeerInfo]()

  def addOrUpdateKnownPeer(address: InetSocketAddress, peerInfo: PeerInfo): Unit = {
    val updatedPeerInfo: PeerInfo = whitelistPersistence.get(address).fold(peerInfo) { dbPeerInfo =>
      val nodeNameOpt: Option[String] = peerInfo.nodeName orElse dbPeerInfo.nodeName
      val connTypeOpt: Option[ConnectionType] = peerInfo.connectionType orElse dbPeerInfo.connectionType
      PeerInfo(peerInfo.lastSeen, nodeNameOpt, connTypeOpt)
    }
    whitelistPersistence += (address -> updatedPeerInfo)
  }

  def knownPeers(): Map[InetSocketAddress, PeerInfo] =
    whitelistPersistence.keys.flatMap(k => whitelistPersistence.get(k).map(v => k -> v)).toMap

  def isEmpty: Boolean = whitelistPersistence.isEmpty

  def remove(address: InetSocketAddress): Unit = whitelistPersistence -= address
}

case class PeerInfo(lastSeen: Long, nodeName: Option[String] = None, connectionType: Option[ConnectionType] = None)