package encry.network

import java.net.{InetAddress, InetSocketAddress}

import akka.actor.{Actor, Props}
import encry.settings.{BlackListSettings, NetworkSettings}

import scala.util.Try

class PK(networkSettings: NetworkSettings,
         blacklistSettings: BlackListSettings) extends Actor {

  var connectedPeers: ConnectedPeersCollection = ConnectedPeersCollection()

  var blackList: BlackList = BlackList(blacklistSettings)

  var knownPeers: Set[InetAddress] = networkSettings.knownPeers
    .collect { case peer: InetSocketAddress if !isSelf(peer) => peer.getAddress }.toSet

  override def receive: Receive = ???

  def isSelf(address: InetSocketAddress): Boolean = Try(address == networkSettings.bindAddress ||
    networkSettings.declaredAddress.contains(address) ||
    InetAddress.getLocalHost.getAddress.sameElements(address.getAddress.getAddress) ||
    InetAddress.getLoopbackAddress.getAddress.sameElements(address.getAddress.getAddress)).getOrElse(true)
}

object PK {
  def props(networkSettings: NetworkSettings,
            blacklistSettings: BlackListSettings): Props = Props(new PK(networkSettings, blacklistSettings))
}
