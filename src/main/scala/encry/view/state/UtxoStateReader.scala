package encry.view.state

import com.google.common.primitives.Ints
import encry.account.{Address, Balance, Portfolio}
import encry.modifiers.state.box._
import encry.view.history.Height
import encry.view.state.index.StateIndexManager
import io.iohk.iodb.Store
import scorex.core.transaction.state.StateReader
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.ADKey
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, NodeParameters, PersistentBatchAVLProver, VersionedIODBAVLStorage}
import scorex.crypto.encode.Base58
import scorex.crypto.hash.{Blake2b256Unsafe, Digest32}

trait UtxoStateReader extends StateIndexManager with StateReader with ScorexLogging {

  implicit val hf: Blake2b256Unsafe = new Blake2b256Unsafe

  val stateStore: Store

  def stateHeight: Height = indexStorage.db.get(StateIndexManager.stateHeightKey)
    .map(d => Height @@ Ints.fromByteArray(d.data)).getOrElse(Height @@ 0)

  // FIXME: Fixed valueSize causes errors during application of boxes of different types.
  private lazy val np =
    NodeParameters(keySize = EncryBox.BoxIdSize, valueSize = AssetBoxSerializer.Size, labelSize = 32)

  protected lazy val storage = new VersionedIODBAVLStorage(stateStore, np)

  protected lazy val persistentProver: PersistentBatchAVLProver[Digest32, Blake2b256Unsafe] =
    PersistentBatchAVLProver.create(
      new BatchAVLProver[Digest32, Blake2b256Unsafe](
        keyLength = EncryBox.BoxIdSize, valueLengthOpt = None), storage).get

  def boxById(boxId: ADKey): Option[EncryBaseBox] =
    boxId.head match {
      case OpenBox.typeId => persistentProver.unauthenticatedLookup(boxId)
        .map(OpenBoxSerializer.parseBytes).flatMap(_.toOption)
      case AssetBox.typeId => persistentProver.unauthenticatedLookup(boxId)
        .map(AssetBoxSerializer.parseBytes).flatMap(_.toOption)
      case _ => None
    }

  def typedBoxById[B <: EncryBaseBox](boxId: ADKey): Option[EncryBaseBox] =
    boxById(boxId) match {
      case Some(bx: B@unchecked) if bx.isInstanceOf[B] => Some(bx)
      case _ => None
    }

  def getAvailableOpenBoxesAt(height: Height): Seq[OpenBox] =
    boxesByAddress(StateIndexManager.openBoxesAddress)
      .map(bxs => bxs.filter(bx => bx.isInstanceOf[OpenBox] &&
        bx.asInstanceOf[OpenBox].proposition.height <= height)
        .map(_.asInstanceOf[OpenBox])).getOrElse(Seq())

  def getOpenBoxIdsAtHeight(height: Height): Seq[ADKey] =
    boxIdsByAddress(StateIndexManager.openBoxesAddress).getOrElse(Seq())

  def getRandomBox: Option[EncryBaseBox] =
    persistentProver.avlProver.randomWalk().map(_._1).flatMap(boxById)

  def boxesByAddress(address: Address): Option[Seq[EncryBaseBox]] =
    boxIdsByAddress(address) match {
      case Some(bxIds) =>
        val bxs = bxIds.foldLeft(Seq[EncryBaseBox]()) { case (buff, id) =>
          boxById(id) match {
            case Some(bx) => buff :+ bx
            case None =>
              log.warn(s"Box: ${Base58.encode(id)} exists in index, but was not found in state.")
              buff
          }
        }
        if (bxs.nonEmpty) Some(bxs) else None
      case _ => None
    }

  def portfolioByAddress(address: Address): Option[Portfolio] =
    boxesByAddress(address) match {
      case Some(bxs) =>
        Some(Portfolio(address,
          Balance @@ bxs.filter(_.isInstanceOf[AssetBox]).map(_.asInstanceOf[AssetBox].amount).sum, Some(bxs)))
      case _ => None
    }
}
