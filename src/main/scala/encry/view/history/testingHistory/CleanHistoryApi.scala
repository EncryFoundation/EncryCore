package encry.view.history.testingHistory

import encry.utils.NetworkTimeProvider
import encry.view.history.History
import org.encryfoundation.common.modifiers.PersistentModifier

trait CleanHistoryApi extends HistoryStorageApi {

  val blockDownloadingProcessor: CleanBlockDownloadingProcessor

  val isHeaderChainSynced: Boolean //todo upstream update

  val timeProvider: NetworkTimeProvider
}