package encry.network

import java.net.InetSocketAddress
import org.encryfoundation.common.network.SyncInfo
import org.encryfoundation.common.utils.TaggedTypes.{ModifierId, ModifierTypeId}

object Messages {

  sealed trait MessageToNetwork
  object MessageToNetwork {
    final case class RequestFromLocal(source: Option[InetSocketAddress],
                                      modifierTypeId: ModifierTypeId,
                                      modifierIds: List[ModifierId]) extends MessageToNetwork
    final case class SendSyncInfo(syncInfo: SyncInfo) extends MessageToNetwork
    final case class ResponseFromLocal(source: InetSocketAddress,
                                       modifierTypeId: ModifierTypeId,
                                       modifiers: Map[ModifierId, Array[Byte]]) extends MessageToNetwork
    final case class BroadcastModifier(modifierTypeId: ModifierTypeId,
                                       modifierId: ModifierId) extends MessageToNetwork
    final case class SendPeers(peers: List[InetSocketAddress], to: InetSocketAddress) extends MessageToNetwork
    final case class BroadcastManifestRequest(manifestId: Array[Byte]) extends MessageToNetwork
    final case class NotifyNodeAboutModifier(source: InetSocketAddress,
                                             modifierTypeId: ModifierTypeId,
                                             modifierId: ModifierId) extends MessageToNetwork
  }
}
