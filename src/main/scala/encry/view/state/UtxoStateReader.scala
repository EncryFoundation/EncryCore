package encry.view.state

import encry.storage.VersionalStorage.{StorageKey, StorageValue}
import encry.utils.CoreTaggedTypes.VersionTag
import encry.view.state.avlTree.AvlTree
import encry.view.state.avlTree.utils.implicits.Instances._
import org.encryfoundation.common.modifiers.state.StateModifierSerializer
import org.encryfoundation.common.modifiers.state.box.EncryBaseBox
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ADKey

trait UtxoStateReader {

  implicit val hf: Algos.HF = Algos.hash

  val tree: AvlTree[StorageKey, StorageValue]

  def version: VersionTag = VersionTag !@@ tree.avlStorage.currentVersion

  def boxById(boxId: ADKey): Option[EncryBaseBox] = tree.get(StorageKey !@@ boxId)
    .map(bytes => StateModifierSerializer.parseBytes(bytes, boxId.head)).flatMap(_.toOption)

  def boxesByIds(ids: Seq[ADKey]): Seq[EncryBaseBox] = ids.foldLeft(Seq[EncryBaseBox]())((acc, id) =>
    boxById(id).map(bx => acc :+ bx).getOrElse(acc))

  def typedBoxById[B <: EncryBaseBox](boxId: ADKey): Option[EncryBaseBox] =
    boxById(boxId) match {
      case Some(bx: B@unchecked) if bx.isInstanceOf[B] => Some(bx)
      case _ => None
    }
}