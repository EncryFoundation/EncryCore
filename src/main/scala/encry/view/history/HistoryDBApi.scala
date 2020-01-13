package encry.view.history

import com.google.common.primitives.Ints
import com.typesafe.scalalogging.StrictLogging
import encry.settings.EncryAppSettings
import encry.storage.VersionalStorage.StorageKey
import encry.view.history.storage.HistoryStorage
import org.encryfoundation.common.modifiers.history.{ Block, Header, Payload }
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ Height, ModifierId, ModifierTypeId }
import scorex.crypto.hash.Digest32

import scala.reflect.ClassTag

trait HistoryDBApi extends StrictLogging {

  val settings: EncryAppSettings

  val historyStorage: HistoryStorage

  lazy val BestHeaderKey: StorageKey =
    StorageKey @@ Array.fill(settings.constants.DigestLength)(Header.modifierTypeId.untag(ModifierTypeId))
  lazy val BestBlockKey: StorageKey =
    StorageKey @@ Array.fill(settings.constants.DigestLength)(-1: Byte)

  private def getModifierById[T: ClassTag](id: ModifierId): Option[T] =
    historyStorage
      .modifierById(id)
      .collect { case m: T => m }

  def getHeightByHeaderIdDB(id: ModifierId): Option[Int] =
    historyStorage
      .get(headerHeightKey(id))
      .map(Ints.fromByteArray)

  def getHeaderByIdDB(id: ModifierId): Option[Header]    = getModifierById[Header](id)
  def getPayloadByIdDB(pId: ModifierId): Option[Payload] = getModifierById[Payload](pId)
  def getBlockByHeaderDB(header: Header): Option[Block] =
    getModifierById[Payload](header.payloadId)
      .map(payload => Block(header, payload))
  def getBlockByHeaderIdDB(id: ModifierId): Option[Block] =
    getHeaderByIdDB(id)
      .flatMap(h => getModifierById[Payload](h.payloadId).map(p => Block(h, p)))

  def getBestHeaderId: Option[ModifierId] = historyStorage.get(BestHeaderKey).map(ModifierId @@ _)
  def getBestHeaderDB: Option[Header]     = getBestHeaderId.flatMap(getHeaderByIdDB)
  def getBestHeaderHeightDB: Int =
    getBestHeaderId
      .flatMap(getHeightByHeaderIdDB)
      .getOrElse(settings.constants.PreGenesisHeight)

  def getBestBlockId: Option[ModifierId] = historyStorage.get(BestBlockKey).map(ModifierId @@ _)
  def getBestBlockDB: Option[Block]      = getBestBlockId.flatMap(getBlockByHeaderIdDB)
  def getBestBlockHeightDB: Int =
    getBestBlockId
      .flatMap(getHeightByHeaderIdDB)
      .getOrElse(settings.constants.PreGenesisHeight)

  def modifierBytesByIdDB(id: ModifierId): Option[Array[Byte]] = historyStorage.modifiersBytesById(id)

  def isModifierDefined(id: ModifierId): Boolean = historyStorage.containsMod(id)

  //todo probably rewrite with indexes collection
  def lastBestBlockHeightRelevantToBestChain(probablyAt: Int): Option[Int] =
    (for {
      headerId <- getBestHeaderIdAtHeightDB(probablyAt)
      header   <- getHeaderByIdDB(headerId) if isModifierDefined(header.payloadId)
    } yield header.height).orElse(lastBestBlockHeightRelevantToBestChain(probablyAt - 1))

  def headerIdsAtHeightDB(height: Int): Option[Seq[ModifierId]] =
    historyStorage
      .get(heightIdsKey(height))
      .map(_.grouped(32).map(ModifierId @@ _).toSeq)

  def getBestHeaderIdAtHeightDB(h: Int): Option[ModifierId] = headerIdsAtHeightDB(h).flatMap(_.headOption)

  def getBestHeaderAtHeightDB(h: Int): Option[Header] = getBestHeaderIdAtHeightDB(h).flatMap(getHeaderByIdDB)

  def isInBestChain(h: Header): Boolean =
    getBestHeaderIdAtHeightDB(h.height)
      .exists(_.sameElements(h.id))

  def isInBestChain(id: ModifierId): Boolean =
    heightOf(id)
      .flatMap(getBestHeaderIdAtHeightDB)
      .exists(_.sameElements(id))

  def getBestHeadersChainScore: BigInt =
    getBestHeaderId.flatMap(scoreOf).getOrElse(BigInt(0)) //todo ?.getOrElse(BigInt(0))?

  def scoreOf(id: ModifierId): Option[BigInt] =
    historyStorage
      .get(headerScoreKey(id))
      .map(d => BigInt(d))

  def heightOf(id: ModifierId): Option[Height] =
    historyStorage
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
