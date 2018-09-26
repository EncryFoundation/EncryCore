package encry.view.state

import encry.modifiers.state.box.EncryBaseBox
import io.iohk.iodb.ByteArrayWrapper
import scala.collection.immutable.SortedMap

class BoxHolder(val boxes: SortedMap[ByteArrayWrapper, EncryBaseBox]) {

  def sortedBoxes: Set[EncryBaseBox] = boxes.keySet.map(k => boxes(k))

  override def toString: String = s"BoxHolder(${boxes.size} boxes inside)"
}

object BoxHolder {
  def apply(bxs: Seq[EncryBaseBox]): BoxHolder =
    new BoxHolder(SortedMap(bxs.map(b => ByteArrayWrapper(b.id) -> b): _*))
}
