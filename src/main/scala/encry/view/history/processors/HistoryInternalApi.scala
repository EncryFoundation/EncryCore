package encry.view.history.processors

import com.google.common.primitives.Ints
import com.typesafe.scalalogging.StrictLogging
import encry.storage.VersionalStorage.StorageKey
import encry.view.history.storage.HistoryStorage
import org.encryfoundation.common.modifiers.history.{Block, Header, Payload}
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{Height, ModifierId, ModifierTypeId}
import org.encryfoundation.common.utils.constants.TestNetConstants
import scorex.crypto.hash.Digest32

trait HistoryInternalApi extends StrictLogging {

  val historyStorage: HistoryStorage

  val BestHeaderKey: StorageKey =
    StorageKey @@ Array.fill(TestNetConstants.DigestLength)(Header.modifierTypeId.untag(ModifierTypeId))
  val BestBlockKey: StorageKey =
    StorageKey @@ Array.fill(TestNetConstants.DigestLength)(-1: Byte)

  private def getModifierById[T](id: ModifierId): Option[T] = historyStorage
    .modifierById(id)
    .collect { case m: T => m }

  def getHeightByHeaderId(id: ModifierId): Option[Int] = historyStorage
    .get(headerHeightKey(id))
    .map(Ints.fromByteArray)

  def getHeaderByIdInternal(id: ModifierId): Option[Header] = getModifierById[Header](id)
  def getPayloadByIdInternal(pId: ModifierId): Option[Payload] = getModifierById[Payload](pId)
  def getBlockByHeaderInternal(header: Header): Option[Block] = getModifierById[Payload](header.payloadId)
    .map(payload => Block(header, payload))
  def getBlockByHeaderIdInternal(id: ModifierId): Option[Block] = getHeaderByIdInternal(id)
    .flatMap(h => getModifierById[Payload](h.payloadId).map(p => Block(h, p)))

  def getBestHeaderId: Option[ModifierId] = historyStorage.get(BestHeaderKey).map(ModifierId @@ _)
  def getBestHeaderInternal: Option[Header] = getBestHeaderId.flatMap(getHeaderByIdInternal)
  def getBestHeaderHeightInternal: Int = getBestHeaderId
    .flatMap(getHeightByHeaderId)
    .getOrElse(TestNetConstants.PreGenesisHeight)

  def getBestBlockId: Option[ModifierId] = historyStorage.get(BestBlockKey).map(ModifierId @@ _)
  def getBestBlockInternal: Option[Block] = getBestBlockId.flatMap(getBlockByHeaderIdInternal)
  def getBestBlockHeightInternal: Int = getBestBlockId
    .flatMap(getHeightByHeaderId)
    .getOrElse(TestNetConstants.PreGenesisHeight)

  def isModifierDefined(id: ModifierId): Boolean = historyStorage.containsMod(id)

  //todo probably rewrite with indexes collection
  def lastBestBlockHeightRelevantToBestChain(probablyAt: Int): Option[Int] = (for {
    headerId <- getBestHeaderIdAtHeight(probablyAt)
    header   <- getHeaderByIdInternal(headerId) if isModifierDefined(header.payloadId)
  } yield header.height).orElse(lastBestBlockHeightRelevantToBestChain(probablyAt - 1))

  def headerIdsAtHeight(height: Int): Seq[ModifierId] = historyStorage
    .get(heightIdsKey(height))
    .map(_.grouped(32).map(ModifierId @@ _).toSeq)
    .getOrElse(Seq.empty[ModifierId])

  def getBestHeaderIdAtHeight(h: Int): Option[ModifierId] = headerIdsAtHeight(h).headOption

  def isInBestChain(h: Header): Boolean = getBestHeaderIdAtHeight(h.height).exists(_.sameElements(h.id))
  def isInBestChain(id: ModifierId): Boolean = heightOf(id).flatMap(getBestHeaderIdAtHeight).exists(_.sameElements(id))

  def getBestHeadersChainScore: BigInt = getBestHeaderId.flatMap(scoreOf).getOrElse(BigInt(0)) //todo ?.getOrElse(BigInt(0))?

  def scoreOf(id: ModifierId): Option[BigInt] = historyStorage
    .get(headerScoreKey(id))
    .map(d => BigInt(d))

  def heightOf(id: ModifierId): Option[Height] = historyStorage
    .get(headerHeightKey(id))
    .map(d => Height @@ Ints.fromByteArray(d))

  def heightIdsKey(height: Int): StorageKey =
    StorageKey @@ Algos.hash(Ints.toByteArray(height)).untag(Digest32)
  def headerScoreKey(id: ModifierId): StorageKey =
    StorageKey @@ Algos.hash("score".getBytes(Algos.charset) ++ id).untag(Digest32)
  def headerHeightKey(id: ModifierId): StorageKey =
    StorageKey @@ Algos.hash("height".getBytes(Algos.charset) ++ id).untag(Digest32)
  def validityKey(id: Array[Byte]): StorageKey =
    StorageKey @@ Algos.hash("validity".getBytes(Algos.charset) ++ id).untag(Digest32)
}