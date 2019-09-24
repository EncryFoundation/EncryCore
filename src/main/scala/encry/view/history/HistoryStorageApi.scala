package encry.view.history

import com.google.common.primitives.Ints
import com.typesafe.scalalogging.StrictLogging
import encry.settings.Settings
import encry.storage.VersionalStorage.StorageKey
import encry.view.history.storage.HistoryStorage
import org.encryfoundation.common.modifiers.history.{Block, Header, Payload}
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{Height, ModifierId, ModifierTypeId}
import scorex.crypto.hash.Digest32
import scala.reflect.ClassTag

trait HistoryStorageApi extends Settings with StrictLogging {

  val storage: HistoryStorage

  val BestHeaderKey: StorageKey =
    StorageKey @@ Array.fill(settings.constants.DigestLength)(Header.modifierTypeId.untag(ModifierTypeId))
  val BestBlockKey: StorageKey =
    StorageKey @@ Array.fill(settings.constants.DigestLength)(-1: Byte)

  private def modifierByIdStorageApi[T: ClassTag](id: ModifierId): Option[T] = storage
    .modifierById(id)
    .collect { case m: T => m }

  def heightByHeaderStorageApi(id: ModifierId): Option[Int] = storage.get(headerHeightKey(id)).map(Ints.fromByteArray)

  def headerByIdStorageApi(id: ModifierId): Option[Header] = modifierByIdStorageApi[Header](id)
  def payloadByIdStorageApi(pId: ModifierId): Option[Payload] = modifierByIdStorageApi[Payload](pId)
  def blockByIdStorageApi(id: ModifierId): Option[Block] = headerByIdStorageApi(id)
    .flatMap(h => modifierByIdStorageApi[Payload](h.payloadId).map(Block(h, _)))

  def bestHeaderIdStorageApi: Option[ModifierId] = storage.get(BestHeaderKey).map(ModifierId @@ _)
  def bestHeaderStorageApi: Option[Header] = bestHeaderIdStorageApi.flatMap(headerByIdStorageApi)
  def bestHeaderHeightStorageApi: Int = bestHeaderIdStorageApi
    .flatMap(heightByHeaderStorageApi)
    .getOrElse(settings.constants.PreGenesisHeight)

  def bestBlockIdStorageApi: Option[ModifierId] = storage.get(BestBlockKey).map(ModifierId @@ _)
  def bestBlockStorageApi: Option[Block] = bestBlockIdStorageApi.flatMap(blockByIdStorageApi)
  def bestBlockHeightStorageApi: Int = bestBlockIdStorageApi
    .flatMap(heightByHeaderStorageApi)
    .getOrElse(settings.constants.PreGenesisHeight)

  def blockByHeaderStorageApi(header: Header): Option[Block] = modifierByIdStorageApi[Payload](header.payloadId)
    .map(Block(header, _))

  def modifierBytesByIdStorageApi(id: ModifierId): Option[Array[Byte]] = storage.modifiersBytesById(id)

  def isModifierDefined(id: ModifierId): Boolean = storage.containsMod(id)

  def lastBestBlockHeightRelevantToBestChain(probablyAt: Int): Option[Int] = (for {
    headerId <- getBestHeaderIdAtHeightStorageApi(probablyAt)
    header   <- headerByIdStorageApi(headerId) if isModifierDefined(header.payloadId)
  } yield header.height).orElse(lastBestBlockHeightRelevantToBestChain(probablyAt - 1))

  def headerIdsAtHeightStorageApi(height: Int): List[ModifierId] = storage.get(heightIdsKey(height))
    .map(_.grouped(32).map(ModifierId @@ _).toList)
    .getOrElse(List.empty[ModifierId])

  def getBestHeaderIdAtHeightStorageApi(h: Int): Option[ModifierId] = headerIdsAtHeightStorageApi(h).headOption

  def isInBestChain(h: Header): Boolean = getBestHeaderIdAtHeightStorageApi(h.height).exists(_.sameElements(h.id))

  def isInBestChain(id: ModifierId): Boolean = heightOf(id)
    .flatMap(getBestHeaderIdAtHeightStorageApi)
    .exists(_.sameElements(id))

  //todo .getOrElse(BigInt(0))
  def getBestHeadersChainScore: BigInt = bestHeaderIdStorageApi.flatMap(scoreOf).getOrElse(BigInt(0))

  def scoreOf(id: ModifierId): Option[BigInt] = storage.get(headerScoreKey(id)).map(BigInt(_))

  def heightOf(id: ModifierId): Option[Height] = storage.get(headerHeightKey(id)).map(Height @@ Ints.fromByteArray(_))

  def heightIdsKey(height: Int): StorageKey = StorageKey @@ Algos.hash(Ints.toByteArray(height)).untag(Digest32)

  def headerScoreKey(id: ModifierId): StorageKey =
    StorageKey @@ Algos.hash("score".getBytes(Algos.charset) ++ id).untag(Digest32)

  def headerHeightKey(id: ModifierId): StorageKey =
    StorageKey @@ Algos.hash("height".getBytes(Algos.charset) ++ id).untag(Digest32)

  def validityKey(id: Array[Byte]): StorageKey =
    StorageKey @@ Algos.hash("validity".getBytes(Algos.charset) ++ id).untag(Digest32)
}