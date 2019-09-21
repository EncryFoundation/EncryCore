package encry.view.history.testingHistory

import com.google.common.primitives.Ints
import com.typesafe.scalalogging.StrictLogging
import encry.settings.Settings
import encry.storage.VersionalStorage.StorageKey
import encry.view.history.storage.HistoryStorage
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.modifiers.history.{Block, Header, Payload}
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ModifierId, ModifierTypeId}
import scorex.crypto.hash.Digest32

import scala.reflect.ClassTag

trait HistoryStorageApi extends Settings with StrictLogging {

  val storage: HistoryStorage

  val BestHeaderKey: StorageKey =
    StorageKey @@ Array.fill(settings.constants.ModifierIdSize)(Header.modifierTypeId.untag(ModifierTypeId))

  val BestBlockKey: StorageKey =
    StorageKey @@ Array.fill(settings.constants.DigestLength)(-1: Byte)

  def isBestBlockDefined: Boolean = bestBlockId.isDefined

  def heightByHeaderStorageApi(id: ModifierId): Option[Int] = storage
    .get(headerHeightKey(id))
    .map(Ints.fromByteArray)

  def bestBlockHeightStorageApi: Int = bestBlockId
    .flatMap(headerByIdStorageApi)
    .map(_.height)
    .getOrElse(settings.constants.GenesisHeight)

  def isModifierDefined(id: ModifierId): Boolean = storage.containsMod(id)

  def isBlockDefined(header: Header): Boolean = isModifierDefined(header.payloadId)

  def blockByHeaderStorageApi(header: Header): Option[Block] = payloadByIdStorageApi(header.payloadId)
    .map(p => Block(header, p))

  private def modifierById[T: ClassTag](id: ModifierId): Option[T] = storage
    .modifierById(id)
    .collect { case m: T => m }

  def payloadByIdStorageApi(pId: ModifierId): Option[Payload] = modifierById[Payload](pId)

  def heightByHeaderIdStorageApi(id: ModifierId): Option[Int] = storage
    .get(headerHeightKey(id))
    .map(Ints.fromByteArray)

  def bestHeaderId: Option[ModifierId] = storage.get(BestHeaderKey).map(ModifierId @@ _)

  def bestBlockId: Option[ModifierId] = storage.get(BestBlockKey).map(ModifierId @@ _)

  def bestHeaderHeightStorageApi: Int = bestHeaderId
    .flatMap(heightByHeaderIdStorageApi)
    .getOrElse(settings.constants.PreGenesisHeight)

  def headerByIdStorageApi(id: ModifierId): Option[Header] = modifierById[Header](id)

  def blockByPayloadStorageApi(payload: Payload): Option[Block] = headerByIdStorageApi(payload.headerId).map(Block(_, payload))

  def headerIdsAtHeightStorageApi(height: Int): List[ModifierId] = storage.get(heightIdsKey(height))
    .map(_.grouped(32).map(ModifierId @@ _).toList)
    .getOrElse(List.empty[ModifierId])

  def scoreOf(id: ModifierId): Option[BigInt] = storage.get(headerScoreKey(id)).map(BigInt(_))

  def bestHeadersChainScore: BigInt = bestHeaderId.flatMap(scoreOf).getOrElse(BigInt(0))

  def bestHeaderIdAtHeightStorageApi(h: Int): Option[ModifierId] = headerIdsAtHeightStorageApi(h).headOption

  def isInBestChain(h: Header): Boolean = bestHeaderIdAtHeightStorageApi(h.height).exists(_.sameElements(h.id))

  def heightIdsKey(height: Int): StorageKey =
    StorageKey @@ Algos.hash(Ints.toByteArray(height)).untag(Digest32)

  def headerScoreKey(id: ModifierId): StorageKey =
    StorageKey @@ Algos.hash("score".getBytes(Algos.charset) ++ id).untag(Digest32)

  def headerHeightKey(id: ModifierId): StorageKey =
    StorageKey @@ Algos.hash("height".getBytes(Algos.charset) ++ id).untag(Digest32)
}