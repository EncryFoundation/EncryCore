package encry.view.history.tmp

import encry.settings.EncryAppSettings
import encry.view.history.BlockDownloadProcessor
import encry.view.history.storage.HistoryStorage
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.modifiers.history.{ Block, Header }
import org.encryfoundation.common.network.SyncInfo
import org.encryfoundation.common.utils.TaggedTypes.ModifierId

trait HistoryState {

  protected[history] val historyStorage: HistoryStorage

  protected[history] val settings: EncryAppSettings

  protected[history] var isHeadersChainSyncedVariable: Boolean

  protected[history] var isFullChainSyncedVariable: Boolean

  protected[history] var lastSyncInfoVariable: SyncInfo = SyncInfo(Seq.empty[ModifierId])

  protected[history] val blockDownloadProcessor: BlockDownloadProcessor =
    BlockDownloadProcessor(settings.node, settings.constants)

  protected[history] final var headersCacheIndexes: Map[Int, List[ModifierId]] =
    Map.empty[Int, List[ModifierId]]

  protected[history] final var blocksCacheIndexes: Map[Int, List[ModifierId]] =
    Map.empty[Int, List[ModifierId]]

  protected[history] final var headersCache: Map[ByteArrayWrapper, Header] =
    Map.empty[ByteArrayWrapper, Header]

  protected[history] final var blocksCache: Map[ByteArrayWrapper, Block] =
    Map.empty[ByteArrayWrapper, Block]

  final def isHeaderChainSynced: Boolean = isHeadersChainSyncedVariable

  final def isFullChainSynced: Boolean = isFullChainSyncedVariable

  final def getLastSyncInfo: SyncInfo = lastSyncInfoVariable

}
