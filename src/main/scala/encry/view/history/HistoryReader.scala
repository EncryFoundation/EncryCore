package encry.view.history

import cats.syntax.option._
import com.google.common.primitives.Ints
import encry.modifiers.history.HeaderChain
import encry.settings.EncryAppSettings
import encry.storage.VersionalStorage.StorageKey
import encry.view.history.storage.HistoryStorage
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.modifiers.history.{
  Block,
  BlockProtoSerializer,
  Header,
  HeaderProtoSerializer,
  Payload
}
import org.encryfoundation.common.utils.TaggedTypes.{ ModifierId, ModifierTypeId }
import encry.view.history.utils.instances.ModifierIdWrapper
import encry.view.history.utils.syntax.wrapper._
import org.encryfoundation.common.utils.Algos
import scorex.crypto.hash.Digest32

import scala.reflect.ClassTag

/**
 * The idea of this interface is to use it instead of the history implementation in places where
 * full history functionality is excessive but some read only operations is needed.
 * Also it is the frame for the history implementation.
 */
abstract class HistoryReader private (
  historyStorage: HistoryStorage,
  settings: EncryAppSettings
) {

  private final var headersCache: Map[ByteArrayWrapper, Header] =
    Map.empty[ByteArrayWrapper, Header]

  private final var blocksCache: Map[ByteArrayWrapper, Block] =
    Map.empty[ByteArrayWrapper, Block]

  //todo was settings.constants.DigestLength, become settings.constants.ModifierIdSize
  final val BestHeaderKey: StorageKey =
    StorageKey @@ Array.fill(settings.constants.ModifierIdSize)(Header.modifierTypeId.untag(ModifierTypeId))

  //todo was settings.constants.DigestLength, become settings.constants.ModifierIdSize
  final val BestBlockKey: StorageKey =
    StorageKey @@ Array.fill(settings.constants.ModifierIdSize)(-1: Byte)

  private final def getModifierById[T: ClassTag](id: ModifierId): Option[T] =
    historyStorage
      .modifierById(id)
      .collect { case m: T => m }

  final def getHeaderById(id: ModifierId): Option[Header] =
    headersCache
      .get(id.wrap)
      .orElse(blocksCache.get(id.wrap).map(_.header))
      .orElse(getModifierById[Header](id))

  final def getBlockByHeader(header: Header): Option[Block] =
    blocksCache
      .get(header.id.wrap)
      .orElse(getModifierById[Payload](header.payloadId).map(Block(header, _)))

  final def getBlockByHeaderId(id: ModifierId): Option[Block] =
    getHeaderById(id)
      .flatMap(getBlockByHeader)

  final def getBlockByPayload(payload: Payload): Option[Block] =
    headersCache
      .get(payload.headerId.wrap)
      .map(Block(_, payload))
      .orElse(blocksCache.get(payload.headerId.wrap))
      .orElse(getHeaderById(payload.headerId).flatMap(Block(_, payload).some))

  final def getBestHeaderId: Option[ModifierId] =
    historyStorage
      .get(BestHeaderKey)
      .map(ModifierId @@ _)

  final def getBestBlockId: Option[ModifierId] =
    historyStorage
      .get(BestBlockKey)
      .map(ModifierId @@ _)

  final def getBestHeader: Option[Header] =
    getBestHeaderId.flatMap { id: ModifierId =>
      headersCache
        .get(id.wrap)
        .orElse(blocksCache.get(id.wrap).map(_.header))
        .orElse(getHeaderById(id))
    }

  final def getBestBlock: Option[Block] =
    getBestBlockId.flatMap { id: ModifierId =>
      blocksCache
        .get(id.wrap)
        .orElse(getBlockByHeaderId(id))
    }

  final def getHeightByHeaderId(id: ModifierId): Option[Int] =
    headersCache
      .get(id.wrap)
      .map(_.height)
      .orElse(
        blocksCache
          .get(id.wrap)
          .map(_.header.height)
      )
      .orElse(
        historyStorage
          .get(headerHeightKey(id))
          .map(Ints.fromByteArray)
      )

  final def getBestHeaderHeight: Int =
    getBestHeaderId.flatMap { id: ModifierId =>
      headersCache
        .get(id.wrap)
        .map(_.height)
        .orElse(blocksCache.get(id.wrap).map(_.header.height))
        .orElse(getHeightByHeaderId(id))
    }.getOrElse(settings.constants.PreGenesisHeight)

  final def getBestBlockHeight: Int =
    getBestBlockId.flatMap { id: ModifierId =>
      blocksCache.get(id.wrap).map(_.header.height).orElse(getHeightByHeaderId(id))
    }.getOrElse(settings.constants.PreGenesisHeight)

  final def getHeaderOfBestBlock: Option[Header] =
    getBestBlockId.flatMap { id: ModifierId =>
      headersCache
        .get(id.wrap)
        .orElse(blocksCache.get(id.wrap).map(_.header))
        .orElse(getHeaderById(id))
    }

  final def headerIdsAtHeightDB(height: Int): Option[Seq[ModifierId]] = historyStorage
    .get(heightIdsKey(height))
    .map(_.grouped(32).map(ModifierId @@ _).toSeq)

  final def getBestHeaderIdAtHeightDB(h: Int): Option[ModifierId] = headerIdsAtHeightDB(h).flatMap(_.headOption)

  final def getBestHeaderAtHeightDB(h: Int): Option[Header] = getBestHeaderIdAtHeightDB(h).flatMap(getHeaderByIdDB)

  def getBestHeaderAtHeight(h: Int): Option[Header] = getBestHeaderAtHeightDB(h)

  def isBestBlockDefined: Boolean =
    getBestBlockId.map(id => blocksCache.contains(ByteArrayWrapper(id))).isDefined ||
      getHeaderOfBestBlock.map(h => isModifierDefined(h.payloadId)).isDefined

  def isBlockDefined(header: Header): Boolean =
    blocksCache.get(ByteArrayWrapper(header.id)).isDefined || isModifierDefined(header.payloadId)

  def isHeaderDefined(id: ModifierId): Boolean =
    headersCache.get(ByteArrayWrapper(id)).isDefined ||
      blocksCache.get(ByteArrayWrapper(id)).isDefined ||
      isModifierDefined(id)

  def getBestHeaderIdAtHeight(h: Int): Option[ModifierId] = getBestHeaderIdAtHeightDB(h)
  def headerIdsAtHeight(height: Int): Seq[ModifierId] =
    headerIdsAtHeightDB(height)
      .getOrElse(Seq.empty[ModifierId])

  def modifierBytesById(id: ModifierId): Option[Array[Byte]] =
    headersCache
      .get(ByteArrayWrapper(id))
      .map(h => HeaderProtoSerializer.toProto(h).toByteArray)
      .orElse(blocksCache.get(ByteArrayWrapper(id)).map(b => BlockProtoSerializer.toProto(b).toByteArray))
      .orElse(modifierBytesByIdDB(id))

  def lastHeaders(count: Int): HeaderChain =
    getBestHeader
      .map(bestHeader => headerChainBack(count, bestHeader, _ => false))
      .getOrElse(HeaderChain.empty)

  def getHeaderIds(count: Int, offset: Int = 0): Seq[ModifierId] =
    (offset until (count + offset))
      .flatMap(getBestHeaderIdAtHeight)

  final def heightIdsKey(height: Int): StorageKey =
    StorageKey @@ Algos.hash(Ints.toByteArray(height)).untag(Digest32)

  final def headerScoreKey(id: ModifierId): StorageKey =
    StorageKey @@ Algos.hash("score".getBytes(Algos.charset) ++ id).untag(Digest32)

  final def headerHeightKey(id: ModifierId): StorageKey =
    StorageKey @@ Algos.hash("height".getBytes(Algos.charset) ++ id).untag(Digest32)

  final def validityKey(id: Array[Byte]): StorageKey =
    StorageKey @@ Algos.hash("validity".getBytes(Algos.charset) ++ id).untag(Digest32)

}

object HistoryReader {}
