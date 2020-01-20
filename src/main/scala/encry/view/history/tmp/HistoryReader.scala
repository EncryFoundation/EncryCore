package encry.view.history.tmp

import com.google.common.primitives.Ints
import encry.storage.VersionalStorage.StorageKey
import encry.view.history.utils.instances.ModifierIdWrapper
import encry.view.history.utils.syntax.wrapper._
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.modifiers.history._
import org.encryfoundation.common.network.SyncInfo
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ Height, ModifierId, ModifierTypeId }
import scorex.crypto.hash.Digest32

import scala.reflect.ClassTag

/**
 * The idea of this interface is to use it instead of the history implementation in places where
 * full history functionality is excessive but some read only operations is needed.
 * Also it is the frame for the history implementation.
 */
trait HistoryReader extends HistoryState {

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
      .orElse(getHeaderById(payload.headerId).map(Block(_, payload)))

  final def getBestHeaderId: Option[ModifierId] =
    historyStorage
      .get(BestHeaderKey)
      .map(ModifierId @@ _)

  final def getBestBlockId: Option[ModifierId] =
    historyStorage
      .get(BestBlockKey)
      .map(ModifierId @@ _)

  final def getBestHeader: Option[Header] =
    getBestHeaderId.flatMap(
      (id: ModifierId) =>
        headersCache
          .get(id.wrap)
          .orElse(blocksCache.get(id.wrap).map(_.header))
          .orElse(getHeaderById(id))
    )

  final def getBestBlock: Option[Block] =
    getBestBlockId.flatMap { id: ModifierId =>
      blocksCache
        .get(id.wrap)
        .orElse(getBlockByHeaderId(id))
    }

  private def getHeightByHeaderIdDB(id: ModifierId): Option[Int] =
    historyStorage
      .get(headerHeightKey(id))
      .map(Ints.fromByteArray)

  final def getHeightByHeaderId(id: ModifierId): Option[Int] =
    headersCache
      .get(id.wrap)
      .map(_.height)
      .orElse(
        blocksCache
          .get(id.wrap)
          .map(_.header.height)
      )
      .orElse(getHeightByHeaderIdDB(id))

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

  final def headerIdsAtHeight(height: Int): List[ModifierId] =
    historyStorage
      .get(heightIdsKey(height))
      .map(_.grouped(32).map(ModifierId @@ _).toList)
      .getOrElse(List.empty)

  final def getBestHeaderIdAtHeight(h: Int): Option[ModifierId] =
    headerIdsAtHeight(h).headOption

  final def getBestHeaderAtHeight(h: Int): Option[Header] =
    getBestHeaderIdAtHeight(h).flatMap(getHeaderById)

  final def isModifierDefined(id: ModifierId): Boolean = historyStorage.containsMod(id)

  final def isBestBlockDefined: Boolean =
    getBestBlockId.map((id: ModifierId) => blocksCache.contains(id.wrap)).isDefined ||
      getHeaderOfBestBlock.map((h: Header) => isModifierDefined(h.payloadId)).isDefined

  final def isBlockDefined(header: Header): Boolean =
    blocksCache.get(header.id.wrap).isDefined || isModifierDefined(header.payloadId)

  final def isHeaderDefined(id: ModifierId): Boolean =
    headersCache.get(id.wrap).isDefined ||
      blocksCache.get(id.wrap).isDefined ||
      isModifierDefined(id)

  final def modifierBytesById(id: ModifierId): Option[Array[Byte]] =
    headersCache
      .get(id.wrap)
      .map(HeaderProtoSerializer.toProto(_).toByteArray)
      .orElse(blocksCache.get(id.wrap).map(BlockProtoSerializer.toProto(_).toByteArray))
      .orElse(historyStorage.modifiersBytesById(id))

  final def heightOf(id: ModifierId): Option[Height] =
    historyStorage
      .get(headerHeightKey(id))
      .map(d => Height @@ Ints.fromByteArray(d))

  final def isInBestChain(h: Header): Boolean =
    getBestHeaderIdAtHeight(h.height)
      .exists(_.sameElements(h.id))

  final def isInBestChain(id: ModifierId): Boolean =
    heightOf(id)
      .flatMap(getBestHeaderIdAtHeight)
      .exists(_.sameElements(id))

  final def scoreOf(id: ModifierId): Option[BigInt] =
    historyStorage
      .get(headerScoreKey(id))
      .map(d => BigInt(d))

  //todo is getOrElse(BigInt(0)) correct?
  final def getBestHeadersChainScore: BigInt = getBestHeaderId.flatMap(scoreOf).getOrElse(BigInt(0))

  final def lastBestBlockHeightRelevantToBestChain(probablyAt: Int): Option[Int] =
    (for {
      headerId <- getBestHeaderIdAtHeight(probablyAt)
      header   <- getHeaderById(headerId) if isModifierDefined(header.payloadId)
    } yield header.height).orElse(lastBestBlockHeightRelevantToBestChain(probablyAt - 1))

  protected[view] def calculateNewSyncInfo(): Unit =
    lastSyncInfoVariable = SyncInfo(getBestHeader.map { header: Header =>
      ((header.height - settings.network.maxInvObjects + 1) to header.height).flatMap { height: Int =>
        headerIdsAtHeight(height).headOption
      }.toList
    }.getOrElse(List.empty))

  final def heightIdsKey(height: Int): StorageKey =
    StorageKey @@ Algos.hash(Ints.toByteArray(height)).untag(Digest32)

  final def headerScoreKey(id: ModifierId): StorageKey =
    StorageKey @@ Algos.hash("score".getBytes(Algos.charset) ++ id).untag(Digest32)

  final def headerHeightKey(id: ModifierId): StorageKey =
    StorageKey @@ Algos.hash("height".getBytes(Algos.charset) ++ id).untag(Digest32)

  final def validityKey(id: Array[Byte]): StorageKey =
    StorageKey @@ Algos.hash("validity".getBytes(Algos.charset) ++ id).untag(Digest32)

  protected[history] final def addHeaderToCacheIfNecessary(h: Header): Unit =
    if (h.height >= getBestHeaderHeight - settings.constants.MaxRollbackDepth) {
      val newHeadersIdsAtHeaderHeight: List[ModifierId] =
        headersCacheIndexes.getOrElse(h.height, List.empty[ModifierId]) :+ h.id
      headersCacheIndexes = headersCacheIndexes + (h.height -> newHeadersIdsAtHeaderHeight)
      headersCache = headersCache + (h.id.wrap              -> h)
      if (headersCacheIndexes.size > settings.constants.MaxRollbackDepth) {
        headersCacheIndexes.get(getBestHeaderHeight - settings.constants.MaxRollbackDepth).foreach {
          headersIds: List[ModifierId] =>
            val wrappedIds = headersIds.map(_.wrap)
            headersCache = headersCache.filterNot { case (id, _) => wrappedIds.contains(id) }
        }
        headersCacheIndexes = headersCacheIndexes - (getBestHeaderHeight - settings.constants.MaxRollbackDepth)
      }
    }

  protected[history] final def addBlockToCacheIfNecessary(b: Block): Unit =
    if (!blocksCache.contains(b.id.wrap) && (b.header.height >= getBestBlockHeight - settings.constants.MaxRollbackDepth)) {
      val newBlocksIdsAtBlockHeight: List[ModifierId] =
        blocksCacheIndexes.getOrElse(b.header.height, List.empty[ModifierId]) :+ b.id
      blocksCacheIndexes = blocksCacheIndexes + (b.header.height -> newBlocksIdsAtBlockHeight)
      blocksCache = blocksCache + (b.id.wrap                     -> b)
      if (blocksCacheIndexes.size > settings.constants.MaxRollbackDepth) {
        blocksCacheIndexes.get(getBestBlockHeight - settings.constants.MaxRollbackDepth).foreach {
          blocksIds: List[ModifierId] =>
            val wrappedIds: List[ByteArrayWrapper] = blocksIds.map(_.wrap)
            blocksCache = blocksCache.filterNot { case (id, _) => wrappedIds.contains(id) }
        }
        blocksCacheIndexes = blocksCacheIndexes - (getBestBlockHeight - settings.constants.MaxRollbackDepth)
      }
    }
}
