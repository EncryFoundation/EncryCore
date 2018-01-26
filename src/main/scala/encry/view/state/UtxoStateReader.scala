package encry.view.state

import com.google.common.primitives.Ints
import encry.account.{Address, Balance}
import encry.modifiers.state.box._
import encry.view.history.Height
import encry.view.state.index.{Portfolio, StateIndexReader}
import io.iohk.iodb.Store
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.ADKey
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, NodeParameters,
  PersistentBatchAVLProver, VersionedIODBAVLStorage}
import scorex.crypto.encode.Base58
import scorex.crypto.hash.{Blake2b256Unsafe, Digest32}

trait UtxoStateReader extends StateIndexReader with ScorexLogging {

  implicit val hf: Blake2b256Unsafe = new Blake2b256Unsafe

  val stateStore: Store

  def stateHeight: Height = indexStorage.db.get(StateIndexReader.stateHeightKey)
    .map(d => Height @@ Ints.fromByteArray(d.data)).getOrElse(Height @@ 0)

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

  def typedBoxById[BXT <: EncryBaseBox](boxId: ADKey): Option[EncryBaseBox] =
    boxById(boxId) match {
      case Some(bx) => if (bx.isInstanceOf[BXT]) Some(bx) else None
      case _ => None
    }

  def getOpenBoxesAtHeight(height: Height): Seq[OpenBox] =
    boxesByAddress(StateIndexReader.openBoxesKey)
      .map(bxs => bxs.filter(bx => bx.isInstanceOf[OpenBox] &&
        bx.asInstanceOf[OpenBox].proposition.height <= height)
        .map(_.asInstanceOf[OpenBox])).getOrElse(Seq())

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
        Some(Portfolio(address, Balance @@ bxs.filter(_.isInstanceOf[AssetBox]).map(_.value).sum, Some(bxs)))
      case _ => None
    }
}
