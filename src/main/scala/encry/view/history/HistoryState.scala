package encry.view.history

import encry.settings.EncryAppSettings
import encry.view.history.storage.HistoryStorage
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.modifiers.history.{ Block, Header }
import org.encryfoundation.common.network.SyncInfo
import org.encryfoundation.common.utils.TaggedTypes.ModifierId

trait HistoryState {

  protected[view] val historyStorage: HistoryStorage

  protected[view] val settings: EncryAppSettings

  protected[view] var isHeadersChainSyncedVariable: Boolean = false

  protected[view] var isFullChainSyncedVariable: Boolean = settings.node.offlineGeneration

  protected[view] var lastAvailableManifestHeightVariable: Int = 0

  protected[view] var fastSyncInProgressVariable: Boolean =
    settings.snapshotSettings.enableFastSynchronization && !settings.node.offlineGeneration

  protected[history] var lastSyncInfoVariable: SyncInfo = SyncInfo(Seq.empty[ModifierId])

  protected[view] val blockDownloadProcessor: BlockDownloadProcessor =
    BlockDownloadProcessor(settings.node, settings.constants)

  protected[history] final var headersCacheIndexes: Map[Int, List[ModifierId]] =
    Map.empty[Int, List[ModifierId]]

  protected[history] final var blocksCacheIndexes: Map[Int, List[ModifierId]] =
    Map.empty[Int, List[ModifierId]]

  protected[history] final var headersCache: Map[ByteArrayWrapper, Header] =
    Map.empty[ByteArrayWrapper, Header]

  protected[history] final var blocksCache: Map[ByteArrayWrapper, Block] =
    Map.empty[ByteArrayWrapper, Block]

  final def isHeadersChainSynced: Boolean = isHeadersChainSyncedVariable

  final def isFullChainSynced: Boolean = isFullChainSyncedVariable

  final def getLastSyncInfo: SyncInfo = lastSyncInfoVariable

  final def lastAvailableManifestHeight: Int = lastAvailableManifestHeightVariable

  final def fastSyncInProgress: Boolean = fastSyncInProgressVariable

}
