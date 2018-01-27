package encry.view.state

import encry.modifiers.state.box.EncryBaseBox
import io.iohk.iodb.ByteArrayWrapper

import scala.collection.immutable.SortedMap

class BoxHolder(val boxes: SortedMap[ByteArrayWrapper, EncryBaseBox]) {

  def size: Int = boxes.size

  def get(id: ByteArrayWrapper): Option[EncryBaseBox] = boxes.get(id)

  def removeBoxes(ids: Seq[EncryBaseBox]): Unit =
    new BoxHolder(boxes.filterKeys(k => !ids.contains(k)))

  def addBoxes(bs: Seq[EncryBaseBox]): BoxHolder =
    new BoxHolder(boxes ++ bs.map(b => ByteArrayWrapper(b.id) -> b))

  def take(qty: Int): (Seq[EncryBaseBox], BoxHolder) =
    (boxes.take(qty).values.toSeq, new BoxHolder(boxes.drop(qty)))

  def sortedBoxes: Set[EncryBaseBox] = boxes.keySet.map(k => boxes(k))

  override def toString = s"BoxHolder(${boxes.size} boxes inside)"
}

object BoxHolder {
  def apply(bxs: Seq[EncryBaseBox]): BoxHolder =
    new BoxHolder(SortedMap(bxs.map(b => ByteArrayWrapper(b.id) -> b): _*))
}
