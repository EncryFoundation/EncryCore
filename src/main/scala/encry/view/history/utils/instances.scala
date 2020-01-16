package encry.view.history.utils

import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.utils.TaggedTypes.ModifierId

object instances {

  implicit val ModifierIdWrapper: Wrapper[ModifierId] = new Wrapper[ModifierId] {
    override def wrap(t: ModifierId): ByteArrayWrapper = ByteArrayWrapper(t)

    override def unwrap(b: ByteArrayWrapper): ModifierId = ModifierId @@ b.data
  }
}
