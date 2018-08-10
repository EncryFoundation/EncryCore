package encry.view.state

import encry.modifiers.state.StateModifierDeserializer
import encry.modifiers.state.box._
import encry.utils.Logging
import encry.view.history.Height
import io.iohk.iodb.Store
import org.encryfoundation.common.Algos
import scorex.crypto.authds.ADKey
import scorex.crypto.authds.avltree.batch.{NodeParameters, PersistentBatchAVLProver, VersionedIODBAVLStorage}
import scorex.crypto.hash.Digest32

trait UtxoStateReader extends StateReader with Logging {

  implicit val hf: Algos.HF = Algos.hash

  val stateStore: Store

  val height: Height

  private lazy val np: NodeParameters = NodeParameters(keySize = EncryBox.BoxIdSize, valueSize = None, labelSize = 32)

  protected lazy val storage: VersionedIODBAVLStorage[Digest32] = new VersionedIODBAVLStorage(stateStore, np)

  protected val persistentProver: PersistentBatchAVLProver[Digest32, Algos.HF]

  def boxById(boxId: ADKey): Option[EncryBaseBox] = persistentProver.unauthenticatedLookup(boxId)
    .map(bytes => StateModifierDeserializer.parseBytes(bytes, boxId.head)).flatMap(_.toOption)

  def boxesByIds(ids: Seq[ADKey]): Seq[EncryBaseBox] = ids.foldLeft(Seq[EncryBaseBox]())((acc, id) =>
    boxById(id).map(bx => acc :+ bx).getOrElse(acc))

  def typedBoxById[B <: EncryBaseBox](boxId: ADKey): Option[EncryBaseBox] =
    boxById(boxId) match {
      case Some(bx: B@unchecked) if bx.isInstanceOf[B] => Some(bx)
      case _ => None
    }

  def getRandomBox: Option[EncryBaseBox] = persistentProver.avlProver.randomWalk().map(_._1).flatMap(boxById)
}
