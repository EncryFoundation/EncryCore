package encry.network.peer

import java.net.InetSocketAddress
import encry.network.PeerConnectionHandler._
import scala.collection.mutable

//todo: persistence
case class PeerDatabase(filename: Option[String]) {

  private val whitelistPersistence: mutable.Map[InetSocketAddress, PeerInfo] = mutable.Map[InetSocketAddress, PeerInfo]()

  def addOrUpdateKnownPeer(address: InetSocketAddress, peerInfo: PeerInfo): Unit = {
    val updatedPeerInfo: PeerInfo = whitelistPersistence.get(address).fold(peerInfo) { dbPeerInfo =>
      val nodeNameOpt: Option[String] = peerInfo.nodeName orElse dbPeerInfo.nodeName
      val connTypeOpt: Option[ConnectionType] = peerInfo.connectionType orElse dbPeerInfo.connectionType
      PeerInfo(peerInfo.lastSeen, nodeNameOpt, connTypeOpt)
    }
    whitelistPersistence.put(address, updatedPeerInfo)
  }

  def knownPeers(): Map[InetSocketAddress, PeerInfo] =
    whitelistPersistence.keys.flatMap(k => whitelistPersistence.get(k).map(v => k -> v)).toMap

  def isEmpty: Boolean = whitelistPersistence.isEmpty

  def remove(address: InetSocketAddress): Boolean = whitelistPersistence.remove(address).nonEmpty
}

case class PeerInfo(lastSeen: Long, nodeName: Option[String] = None, connectionType: Option[ConnectionType] = None)
