package encry.network

import encry.network.PeerConnectionHandler.ConnectedPeer
import org.encryfoundation.common.network.SyncInfo
import org.encryfoundation.common.utils.TaggedTypes.{ModifierId, ModifierTypeId}

object Messages {

  sealed trait MessageToNetwork
  object MessageToNetwork {
    final case class RequestFromLocal(source: ConnectedPeer,
                                      modifierTypeId: ModifierTypeId,
                                      modifierIds: Seq[ModifierId]) extends MessageToNetwork
    final case class SendSyncInfo(syncInfo: SyncInfo) extends MessageToNetwork
  }
}
