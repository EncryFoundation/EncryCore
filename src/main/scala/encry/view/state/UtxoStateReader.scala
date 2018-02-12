package encry.view.state

import com.google.common.primitives.Ints
import encry.account.{Address, Balance, Portfolio}
import encry.modifiers.state.StateModifierDeserializer
import encry.modifiers.state.box._
import encry.settings.Algos
import encry.view.history.Height
import encry.view.state.index.StateIndexManager
import io.iohk.iodb.Store
import scorex.core.transaction.state.StateReader
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.ADKey
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, NodeParameters, PersistentBatchAVLProver, VersionedIODBAVLStorage}
import scorex.crypto.hash.{Blake2b256Unsafe, Digest32}

trait UtxoStateReader extends StateIndexManager with StateReader with ScorexLogging {

  implicit val hf: Blake2b256Unsafe = new Blake2b256Unsafe

  val stateStore: Store

  def stateHeight: Height = indexStorage.db.get(StateIndexManager.stateHeightKey)
    .map(d => Height @@ Ints.fromByteArray(d.data)).getOrElse(Height @@ 0)

  private lazy val np = NodeParameters(keySize = EncryBox.BoxIdSize, labelSize = 32)

  protected lazy val storage = new VersionedIODBAVLStorage(stateStore, np)

  protected lazy val persistentProver: PersistentBatchAVLProver[Digest32, Blake2b256Unsafe] = {
    val bp = new BatchAVLProver[Digest32, Blake2b256Unsafe](keyLength = 32, valueLengthOpt = None)
    PersistentBatchAVLProver.create(bp, storage).get
  }

  def boxById(boxId: ADKey): Option[EncryBaseBox] = persistentProver.unauthenticatedLookup(boxId)
    .map(bytes => StateModifierDeserializer.parseBytes(bytes, boxId.head)).flatMap(_.toOption)

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
            case Some(bx) =>
              buff :+ bx
            case None =>
              log.warn(s"Box: ${Algos.encode(id)} exists in index, but was not found in state.")
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
          Balance @@ bxs.filter(_.isInstanceOf[AssetBox]).map(_.asInstanceOf[AssetBox].amount).sum, bxs))
      case _ => None
    }
}
