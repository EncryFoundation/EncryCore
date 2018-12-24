package encry.view.state

import encry.avltree.{NodeParameters, PersistentBatchAVLProver, VersionedIODBAVLStorage}
import encry.modifiers.state.StateModifierSerializer
import encry.modifiers.state.box._
import encry.view.history.History.Height
import io.iohk.iodb.Store
import org.encryfoundation.common.Algos
import org.encryfoundation.common.utils.TaggedTypes.ADKey
import scorex.crypto.hash.Digest32

trait UtxoStateReader extends StateReader {

  implicit val hf: Algos.HF = Algos.hash

  val stateStore: Store

  val height: Height

  private lazy val np: NodeParameters = NodeParameters(keySize = EncryBox.BoxIdSize, valueSize = None, labelSize = 32)

  protected lazy val storage: VersionedIODBAVLStorage[Digest32] = new VersionedIODBAVLStorage(stateStore, np)

  protected val persistentProver: PersistentBatchAVLProver[Digest32, Algos.HF]

  def boxById(boxId: ADKey): Option[EncryBaseBox] = persistentProver.unauthenticatedLookup(boxId)
    .map(bytes => StateModifierSerializer.parseBytes(bytes, boxId.head)).flatMap(_.toOption)

  def boxesByIds(ids: Seq[ADKey]): Seq[EncryBaseBox] = ids.foldLeft(Seq[EncryBaseBox]())((acc, id) =>
    boxById(id).map(bx => acc :+ bx).getOrElse(acc))

  def typedBoxById[B <: EncryBaseBox](boxId: ADKey): Option[EncryBaseBox] =
    boxById(boxId) match {
      case Some(bx: B@unchecked) if bx.isInstanceOf[B] => Some(bx)
      case _ => None
    }

  def getRandomBox: Option[EncryBaseBox] = persistentProver.avlProver.randomWalk().map(_._1).flatMap(boxById)
}
