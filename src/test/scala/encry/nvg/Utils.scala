package encry.nvg

import cats.Eq
import cats.syntax.eq._
import encry.network.Messages.MessageToNetwork.RequestFromLocal
import encry.network.NodeViewSynchronizer.ReceivableMessages.OtherNodeSyncingStatus
import org.encryfoundation.common.utils.TaggedTypes.ModifierId

object Utils {
  object instances {
    implicit object ModifierIdEq extends Eq[ModifierId] {
      override def eqv(x: ModifierId, y: ModifierId): Boolean =
        x.sameElements(y)
    }

    implicit object OtherNodeSyncStatusEq extends Eq[OtherNodeSyncingStatus] {
      override def eqv(x: OtherNodeSyncingStatus, y: OtherNodeSyncingStatus): Boolean =
        x.status == y.status && x.remote == y.remote
    }

    implicit object RequestFromLocalEq extends Eq[RequestFromLocal] {
      override def eqv(x: RequestFromLocal, y: RequestFromLocal): Boolean =
        x.modifierIds.size == y.modifierIds.size &&
          x.modifierIds.zip(y.modifierIds).forall { case (id, id1) => id === id1 } &&
          x.source.zip(y.source).forall { case (is1, is2)          => is1 == is2 }
    }
  }

}
