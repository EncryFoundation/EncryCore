package encry.network

import java.net.InetAddress

import encry.network.BlackList.BanType.{PermanentBan, TemporaryBan}
import encry.network.BlackList._
import encry.settings.BlackListSettings

final case class BlackList(settings: BlackListSettings,
                           private val blackList: Map[InetAddress, (BanReason, BanTime, BanType)]) {

  def contains(peer: InetAddress): Boolean = blackList.contains(peer)

  def banPeer(reason: BanReason, peer: InetAddress): BlackList =
    BlackList(settings, blackList.updated(peer, (reason, BanTime(System.currentTimeMillis()), reason match {
      case _ => TemporaryBan
    })))

  def cleanupBlackList: BlackList = BlackList(settings, blackList.filterNot { case (_, (_, banTime, banType)) =>
    banType != PermanentBan && (System.currentTimeMillis() - banTime.time >= settings.banTime.toMillis)
  })

  def remove(peer: InetAddress): BlackList = BlackList(settings, blackList - peer)

  def collect[T](p: (InetAddress, BanReason, BanTime, BanType) => Boolean,
                 f: (InetAddress, BanReason, BanTime, BanType) => T): Seq[T] = blackList
    .collect { case (add, (r, t, bt)) if p(add, r, t, bt) => f(add, r, t, bt) }
    .toSeq

  def getAll: Seq[(InetAddress, (BanReason, BanTime, BanType))] = blackList.toSeq
}

object BlackList {

  sealed trait BanReason
  object BanReason {
    case object SemanticallyInvalidPersistentModifier extends BanReason
    case object SyntacticallyInvalidPersistentModifier extends BanReason
    case object SyntacticallyInvalidTransaction extends BanReason
    case object CorruptedSerializedBytes extends BanReason
    case object ModifierIdInTheNetworkMessageIsNotTheSameAsIdOfModifierInThisMessage extends BanReason
    case object SpamSender extends BanReason
    case object SentPeersMessageWithoutRequest extends BanReason
    case object SentInvForPayload extends BanReason
    case object ExpiredNumberOfConnections extends BanReason
    final case class InvalidNetworkMessage(msgName: String) extends BanReason
    final case class InvalidResponseManifestMessage(error: String) extends BanReason
    final case class InvalidChunkMessage(error: String) extends BanReason
    final case class InvalidManifestHasChangedMessage(error: String) extends BanReason
    case object ExpiredNumberOfReRequestAttempts extends BanReason
    case object ExpiredNumberOfRequests extends BanReason
    final case class InvalidStateAfterFastSync(error: String) extends BanReason
    final case class PreSemanticInvalidModifier(error: String) extends BanReason
  }

  sealed trait BanType
  object BanType {
    case object PermanentBan extends BanType
    case object TemporaryBan extends BanType
  }

  final case class BanTime(time: Long) extends AnyVal

  def apply(settings: BlackListSettings): BlackList =
    BlackList(settings, Map.empty[InetAddress, (BanReason, BanTime, BanType)])
}