package encry.nvg

import cats.Eq
import encry.network.NodeViewSynchronizer.ReceivableMessages.OtherNodeSyncingStatus
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
import cats.instances.option._
import cats.instances.int._
import cats.instances.byte._
import cats.syntax.eq._

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
  }

}
