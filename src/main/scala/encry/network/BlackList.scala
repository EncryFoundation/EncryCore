package encry.network

import java.net.InetAddress
import encry.network.BlackList._
import encry.settings.EncryAppSettings

final class BlackList(settings: EncryAppSettings) {

  private var blackList: Map[InetAddress, (BanReason, BanTime, BanType)] = Map.empty

  def banPeer(reason: BanReason, peer: InetAddress): Unit = {
    val banType: BanType = reason match {
      case _ => TemporaryBan
    }
    blackList = blackList.updated(peer, (reason, BanTime(System.currentTimeMillis()), banType))
  }

  def cleanupBlackList(): Unit = blackList = blackList.filterNot { case (_, (_, banTime, banType)) =>
    banType != PermanentBan && (System.currentTimeMillis() - banTime.time >= settings.blackList.banTime.toMillis)
  }

  def getBannedPeers: Set[InetAddress] = blackList.keySet

  def getBannedPeersAndReasons: Map[InetAddress, BanReason] = blackList.map(p => p._1 -> p._2._1)

  def contains(peer: InetAddress): Boolean = blackList.contains(peer)

  def remove(peer: InetAddress): Unit = blackList -= peer

}

object BlackList {

  sealed trait BanReason
  case object SemanticallyInvalidPersistentModifier extends BanReason
  case object SyntacticallyInvalidPersistentModifier extends BanReason
  case object SyntacticallyInvalidTransaction extends BanReason
  case object CorruptedSerializedBytes extends BanReason
  case object SpamSender extends BanReason
  case object SentPeersMessageWithoutRequest extends BanReason
  case object SentInvForPayload extends BanReason
  case object SentNetworkMessageWithTooManyModifiers extends BanReason
  final case class InvalidNetworkMessage(msgName: String) extends BanReason
  case object ExpiredNumberOfConnections extends BanReason

  final case class BanTime(time: Long) extends AnyVal

  sealed trait BanType
  case object PermanentBan extends BanType
  case object TemporaryBan extends BanType

}