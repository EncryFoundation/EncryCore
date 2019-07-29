package encry.view.history

import com.google.common.primitives.Ints
import com.typesafe.scalalogging.StrictLogging
import encry.storage.VersionalStorage.StorageKey
import encry.view.history.storage.HistoryStorage
import org.encryfoundation.common.modifiers.history.{Block, Header, Payload}
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{Height, ModifierId, ModifierTypeId}
import org.encryfoundation.common.utils.constants.TestNetConstants
import scorex.crypto.hash.Digest32

trait HistoryAPI extends StrictLogging {

  val history: HistoryStorage

  val BestHeaderKey: StorageKey =
    StorageKey @@ Array.fill(TestNetConstants.DigestLength)(Header.modifierTypeId.untag(ModifierTypeId))
  val BestBlockKey: StorageKey =
    StorageKey @@ Array.fill(TestNetConstants.DigestLength)(-1: Byte)

  /**
    * @param id - modifier's id
    * @tparam T - type of modifier which want to take
    * @return - Some(Modifier: T) if modifier with id 'id' and type 'T' contains in history otherwise None
    */
  private def getModifierById[T](id: ModifierId): Option[T] = history
    .modifierById(id)
    .collect { case m: T => m }

  /**
    * @param id - id of modifier which height want to find
    * @return Some(height: Int) if such modifier's height key contains in history otherwise None
    */
  def getModifierHeightById(id: ModifierId): Option[Int] = history
    .get(modifierHeightKey(id))
    .map(Ints.fromByteArray)

  /**
    * @param id - block's id
    * @return Some(Block) if header and payload of desired block contains in history otherwise None
    */
  def getBlockById(id: ModifierId): Option[Block] = getModifierById[Header](id)
    .flatMap(h => getModifierById[Payload](h.payloadId).map(p => Block(h, p)))

  /**
    * @param id - header's id
    * @return Some(Header) if desired header contains in history otherwise None
    */
  def getHeaderById(id: ModifierId): Option[Header] = getModifierById[Header](id)

  def getBestBlockIdOpt: Option[ModifierId] = history.get(BestBlockKey).map(ModifierId @@ _)
  def getBestBlockOpt: Option[Block] = getBestBlockIdOpt.flatMap(getBlockById)
  def getBestBlockHeight: Int = getBestBlockIdOpt
    .flatMap(getModifierHeightById)
    .getOrElse(TestNetConstants.PreGenesisHeight)

  def getBestHeaderIdOpt: Option[ModifierId] = history.get(BestHeaderKey).map(ModifierId @@ _)
  def getBestHeaderOpt: Option[Header] = getBestHeaderIdOpt.flatMap(getHeaderById)
  def getBestHeaderHeight: Int = getBestHeaderIdOpt
    .flatMap(getModifierHeightById)
    .getOrElse(TestNetConstants.PreGenesisHeight)

  def scoreOf(id: ModifierId): Option[BigInt] = history
    .get(headerScoreKey(id))
    .map(n => BigInt(n))

  def heightOf(id: ModifierId): Option[Height] = history
    .get(modifierHeightKey(id))
    .map(n => Height @@ Ints.fromByteArray(n))

  def isModifierDefined(id: ModifierId): Boolean = history.containsMod(id)

  def bestHeaderIdAtHeight(height: Int): Option[ModifierId] = headerIdsAtHeight(height).headOption

  /**
    * @param height - block height
    * @return ids of headers on chosen height.
    *         Seq.empty we don't have any headers on this height
    *         single id if no forks on this height
    *         multiple ids if there are forks at chosen height.
    *         First id is always from the best headers chain.
    */

  def headerIdsAtHeight(height: Int): Seq[ModifierId] = history
    .get(heightIdKey(height))
    .map(elem => elem.grouped(32).map(ModifierId @@ _).toSeq)
    .getOrElse(Seq.empty)

  def getHeaderOfBestBlock: Option[Header] = getBestBlockIdOpt.flatMap(getHeaderById)

  def getBestHeadersChainScore: BigInt = scoreOf(getBestHeaderIdOpt.get).getOrElse(BigInt(0)) //todo check getOrElse

  def isInBestChain(id: ModifierId): Boolean = heightOf(id)
    .flatMap(h => bestHeaderIdAtHeight(h))
    .exists(_.sameElements(id))

  def isInBestChain(h: Header): Boolean = bestHeaderIdAtHeight(h.height).exists(_ sameElements h.id)

  def lastBestBlockHeightRelevantToBestChain(probablyAt: Int): Option[Int] = (for {
    headerId <- bestHeaderIdAtHeight(probablyAt)
    header   <- getHeaderById(headerId) if isModifierDefined(header.payloadId)
  } yield header.height).orElse(lastBestBlockHeightRelevantToBestChain(probablyAt - 1))

  def getModifierBytesByIdOpt(id: ModifierId): Option[Array[Byte]] = history.modifiersBytesById(id)

  def heightIdKey(height: Int): StorageKey =
    StorageKey @@ Algos.hash(Ints.toByteArray(height)).untag(Digest32)
  def headerScoreKey(id: ModifierId): StorageKey =
    StorageKey @@ Algos.hash("score".getBytes(Algos.charset) ++ id).untag(Digest32)
  def modifierHeightKey(id: ModifierId): StorageKey =
    StorageKey @@ Algos.hash("height".getBytes(Algos.charset) ++ id).untag(Digest32)
  def validityKey(id: Array[Byte]): StorageKey =
    StorageKey @@ Algos.hash("validity".getBytes(Algos.charset) ++ id).untag(Digest32)
}