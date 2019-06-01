package encry.network

import java.net.InetAddress
import encry.network.BlackList._
import encry.settings.EncryAppSettings

final class BlackList(settings: EncryAppSettings) {

  private var blackList: Map[InetAddress, (BanReason, BanTime, BanType)] = Map.empty

  private def permanentBan(reason: BanReason, peer: InetAddress): Unit =
    blackList = blackList.updated(peer, (reason, BanTime(System.currentTimeMillis()), PermanentBan))

  private def temporaryBan(reason: BanReason, peer: InetAddress): Unit =
    blackList = blackList.updated(peer, (reason, BanTime(System.currentTimeMillis()), TemporaryBan))

  def banPeer(reason: BanReason, peer: InetAddress): Unit = reason match {
    case SpamSender => temporaryBan(reason, peer)
    case _ => permanentBan(reason, peer)
  }

  def cleanupBlackList(): Unit = blackList = blackList.filter { case (_, (_, banTime, banType)) =>
    banType != PermanentBan && banTime.time <= System.currentTimeMillis() + 600000
  }

  def getBannedPeers: Set[InetAddress] = blackList.keySet

  def contains(peer: InetAddress): Boolean = blackList.contains(peer)

}

object BlackList {

  sealed trait BanReason
  final case object SemanticallyInvalidModifier extends BanReason
  final case object SyntacticallyInvalidModifier extends BanReason
  final case object SpamSender extends BanReason

  final case class BanTime(time: Long) extends AnyVal

  sealed trait BanType
  final case object PermanentBan extends BanType
  final case object TemporaryBan extends BanType

}