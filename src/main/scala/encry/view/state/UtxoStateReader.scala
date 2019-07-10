package encry.view.state

import encry.settings.EncryAppSettings
import encry.storage.VersionalStorage
import encry.storage.VersionalStorage.StorageKey
import encry.utils.CoreTaggedTypes.VersionTag
import org.encryfoundation.common.modifiers.state.StateModifierSerializer
import org.encryfoundation.common.modifiers.state.box.{EncryBaseBox, EncryBox}
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ADKey, Height}
import scorex.crypto.hash.Digest32

trait UtxoStateReader {

  implicit val hf: Algos.HF = Algos.hash

  val height: Height

  protected val storage: VersionalStorage

  def version: VersionTag = VersionTag !@@ storage.currentVersion

  def boxById(boxId: ADKey): Option[EncryBaseBox] = storage.get(StorageKey !@@ boxId)
    .map(bytes => StateModifierSerializer.parseBytes(bytes, boxId.head)).flatMap(_.toOption)

  def boxesByIds(ids: Seq[ADKey]): Seq[EncryBaseBox] = ids.foldLeft(Seq[EncryBaseBox]())((acc, id) =>
    boxById(id).map(bx => acc :+ bx).getOrElse(acc))

  def typedBoxById[B <: EncryBaseBox](boxId: ADKey): Option[EncryBaseBox] =
    boxById(boxId) match {
      case Some(bx: B@unchecked) if bx.isInstanceOf[B] => Some(bx)
      case _ => None
    }
}
