package encry.view.history.tmp

import encry.settings.EncryAppSettings
import encry.storage.VersionalStorage.StorageKey
import encry.view.history.storage.HistoryStorage
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.modifiers.history.{ Block, Header }
import org.encryfoundation.common.utils.TaggedTypes.{ ModifierId, ModifierTypeId }
import encry.view.history.utils.instances.ModifierIdWrapper
import encry.view.history.utils.syntax.wrapper._

trait HistoryState {

  protected[history] val historyStorage: HistoryStorage

  protected[history] val settings: EncryAppSettings

  //todo was settings.constants.DigestLength, become settings.constants.ModifierIdSize
  final val BestHeaderKey: StorageKey =
    StorageKey @@ Array.fill(settings.constants.ModifierIdSize)(Header.modifierTypeId.untag(ModifierTypeId))

  //todo was settings.constants.DigestLength, become settings.constants.ModifierIdSize
  final val BestBlockKey: StorageKey =
    StorageKey @@ Array.fill(settings.constants.ModifierIdSize)(-1: Byte)

  private var isHeadersChainSyncedVariable: Boolean = false

  private var isFullChainSyncedVariable: Boolean = false

  private final var headersCacheIndexes: Map[Int, List[ModifierId]] =
    Map.empty[Int, List[ModifierId]]

  private final var blocksCacheIndexes: Map[Int, List[ModifierId]] =
    Map.empty[Int, List[ModifierId]]

  private final var headersCache: Map[ByteArrayWrapper, Header] =
    Map.empty[ByteArrayWrapper, Header]

  private final var blocksCache: Map[ByteArrayWrapper, Block] =
    Map.empty[ByteArrayWrapper, Block]

  final def isHeaderChainSynced: Boolean = isHeadersChainSyncedVariable

  final def isFullChainSynced: Boolean = isFullChainSyncedVariable

  protected[history] final def changeIsHeaderChainSyncedVariable(newValue: Boolean): Unit =
    isHeadersChainSyncedVariable = newValue

  protected[history] final def changeIsFullChainSyncedVariable(newValue: Boolean): Unit =
    isFullChainSyncedVariable = newValue

  protected[history] final def addHeaderToCacheIfNecessary(h: Header): Unit =
    if (h.height >= getBestHeaderHeight - settings.constants.MaxRollbackDepth) {
      val newHeadersIdsAtHeaderHeight: List[ModifierId] =
        headersCacheIndexes.getOrElse(h.height, List.empty[ModifierId]) :+ h.id
      headersCacheIndexes = headersCacheIndexes + (h.height -> newHeadersIdsAtHeaderHeight)
      headersCache = headersCache + (h.id.wrap -> h)
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
      blocksCache = blocksCache + (b.id.wrap -> b)
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
