package encry.view.state

import encry.modifiers.state.box.EncryBaseBox
import io.iohk.iodb.ByteArrayWrapper

import scala.collection.immutable.SortedMap

class BoxHolder(val boxes: SortedMap[ByteArrayWrapper, EncryBaseBox]) {

  def size = boxes.size

  def get(id: ByteArrayWrapper): Option[EncryBaseBox] = boxes.get(id)

  def removeBoxes(ids: Seq[EncryBaseBox]): Unit =
    new BoxHolder(boxes.filterKeys(k => !ids.contains(k)))

  def addBoxes(bs: Seq[EncryBaseBox]): BoxHolder =
    new BoxHolder(boxes ++ bs.map(b => ByteArrayWrapper(b.id) -> b))

  def take(howMany: Int): (Seq[EncryBaseBox], BoxHolder) =
    (boxes.take(howMany).values.toSeq, new BoxHolder(boxes.drop(howMany)))

  def sortedBoxes: Set[EncryBaseBox] = boxes.keySet.map(k => boxes(k))

  override def toString = s"BoxHolder(${boxes.size} boxes inside)"
}

object BoxHolder {
  def apply(initialBoxes: Seq[EncryBaseBox]): BoxHolder =
    new BoxHolder(SortedMap(initialBoxes.map(b => ByteArrayWrapper(b.id) -> b): _*))
}
