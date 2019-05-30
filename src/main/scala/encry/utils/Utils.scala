package encry.utils

import com.google.common.primitives.Longs
import org.encryfoundation.common.network.BasicMessagesRepo.BasicMsgDataTypes.InvData
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ModifierId, ModifierTypeId}

object Utils {

  def nonceFromDigest(digest: Array[Byte]): Long = Longs.fromByteArray(Algos.hash(digest).take(8))

  def idsToString(ids: Seq[(ModifierTypeId, ModifierId)]): String = (ids.headOption, ids.lastOption) match {
    case (Some(f), Some(l)) if f._2 sameElements l._2 => s"[(${f._1},${Algos.encode(f._2)})]"
    case (Some(f), Some(l)) => s"[(${f._1},${Algos.encode(f._2)})..(${l._1},${Algos.encode(l._2)})]"
    case _ => "[]"
  }

  def idsToString(modifierType: ModifierTypeId, ids: Seq[ModifierId]): String =
    idsToString(ids.map(id => (modifierType, id)))

  def idsToString(invData: InvData): String = idsToString(invData._1, invData._2)
}