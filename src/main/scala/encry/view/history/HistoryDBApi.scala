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

  protected[view] val historyStorage: HistoryStorage

  protected[history] final lazy val BestHeaderKey: StorageKey =
    StorageKey @@ Array.fill(settings.constants.DigestLength)(Header.modifierTypeId.untag(ModifierTypeId))
  protected[history] final lazy val BestBlockKey: StorageKey =
    StorageKey @@ Array.fill(settings.constants.DigestLength)(-1: Byte)

  private def getModifierById[T: ClassTag](id: ModifierId): Option[T] =
    historyStorage
      .modifierById(id)
      .collect { case m: T => m }

  final def getHeightByHeaderIdDB(id: ModifierId): Option[Int] =
    historyStorage
      .get(headerHeightKey(id))
      .map(Ints.fromByteArray)

  final def getHeaderByIdDB(id: ModifierId): Option[Header]    = getModifierById[Header](id)
  final def getPayloadByIdDB(pId: ModifierId): Option[Payload] = getModifierById[Payload](pId)
  final def getBlockByHeaderIdDB(id: ModifierId): Option[Block] =
    getHeaderByIdDB(id)
      .flatMap(h => getModifierById[Payload](h.payloadId).map(p => Block(h, p)))

  final def getBestHeaderId: Option[ModifierId] =
    historyStorage.get(BestHeaderKey).map(ModifierId @@ _)

  final def getBestBlockId: Option[ModifierId] =
    historyStorage.get(BestBlockKey).map(ModifierId @@ _)

  final def modifierBytesByIdDB(id: ModifierId): Option[Array[Byte]] =
    historyStorage.modifiersBytesById(id)

  final def isModifierDefined(id: ModifierId): Boolean = historyStorage.containsMod(id)

  //todo probably rewrite with indexes collection
  final def lastBestBlockHeightRelevantToBestChain(probablyAt: Int): Option[Int] =
    (for {
      headerId <- getBestHeaderIdAtHeightDB(probablyAt)
      header   <- getHeaderByIdDB(headerId) if isModifierDefined(header.payloadId)
    } yield header.height).orElse(lastBestBlockHeightRelevantToBestChain(probablyAt - 1))

  final def headerIdsAtHeightDB(height: Int): Option[Seq[ModifierId]] =
    historyStorage
      .get(heightIdsKey(height))
      .map(_.grouped(32).map(ModifierId @@ _).toSeq)

  final def getBestHeaderIdAtHeightDB(h: Int): Option[ModifierId] =
    headerIdsAtHeightDB(h).flatMap(_.headOption)

  final def getBestHeaderAtHeightDB(h: Int): Option[Header] =
    getBestHeaderIdAtHeightDB(h).flatMap(getHeaderByIdDB)

  final def isInBestChain(h: Header): Boolean =
    getBestHeaderIdAtHeightDB(h.height)
      .exists(_.sameElements(h.id))

  final def isInBestChain(id: ModifierId): Boolean =
    heightOf(id)
      .flatMap(getBestHeaderIdAtHeightDB)
      .exists(_.sameElements(id))

  final def getBestHeadersChainScore: BigInt =
    getBestHeaderId.flatMap(scoreOf).getOrElse(BigInt(0)) //todo ?.getOrElse(BigInt(0))?

  final def scoreOf(id: ModifierId): Option[BigInt] =
    historyStorage
      .get(headerScoreKey(id))
      .map(d => BigInt(d))

  final def heightOf(id: ModifierId): Option[Height] =
    historyStorage
      .get(headerHeightKey(id))
      .map(d => Height @@ Ints.fromByteArray(d))

  final def heightIdsKey(height: Int): StorageKey =
    StorageKey @@ Algos.hash(Ints.toByteArray(height)).untag(Digest32)
  final def headerScoreKey(id: ModifierId): StorageKey =
    StorageKey @@ Algos.hash("score".getBytes(Algos.charset) ++ id).untag(Digest32)
  final def headerHeightKey(id: ModifierId): StorageKey =
    StorageKey @@ Algos.hash("height".getBytes(Algos.charset) ++ id).untag(Digest32)
  final def validityKey(id: Array[Byte]): StorageKey =
    StorageKey @@ Algos.hash("validity".getBytes(Algos.charset) ++ id).untag(Digest32)
}
