package encry.view.state

import encry.modifiers.state.box._
import encry.view.history.Height
import io.iohk.iodb.Store
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.ADKey
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, NodeParameters, PersistentBatchAVLProver, VersionedIODBAVLStorage}
import scorex.crypto.hash.{Blake2b256Unsafe, Digest32}

trait UtxoStateReader extends ScorexLogging {

  implicit val hf: Blake2b256Unsafe = new Blake2b256Unsafe

  val stateStore: Store

  private lazy val np = NodeParameters(keySize = EncryBox.BoxIdSize, valueSize = AssetBoxSerializer.Size, labelSize = 32)

  protected lazy val storage = new VersionedIODBAVLStorage(stateStore, np)

  protected lazy val persistentProver: PersistentBatchAVLProver[Digest32, Blake2b256Unsafe] =
    PersistentBatchAVLProver.create(
      new BatchAVLProver[Digest32, Blake2b256Unsafe](
        keyLength = EncryBox.BoxIdSize, valueLengthOpt = None),
      storage).get

  def typedBoxById(boxId: ADKey): Option[EncryBaseBox] = {
    boxId.head match {
      case OpenBox.typeId => persistentProver.unauthenticatedLookup(boxId)
        .map(OpenBoxSerializer.parseBytes).flatMap(_.toOption)
      case AssetBox.typeId => persistentProver.unauthenticatedLookup(boxId)
        .map(AssetBoxSerializer.parseBytes).flatMap(_.toOption)
    }
  }

  def getOpenBoxesAtHeight(height: Height): IndexedSeq[CoinbaseBox] = ???

  def boxesOf(proposition: Proposition): Seq[Box[proposition.type]] = ???

  def randomBox(): Option[EncryBaseBox] =
    persistentProver.avlProver.randomWalk().map(_._1).flatMap(typedBoxById)
}
