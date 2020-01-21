package encry.view.history.processors

import encry.consensus.HistoryConsensus.ProgressInfo
import encry.view.history.HistoryApi
import org.encryfoundation.common.modifiers.history.Header

trait HistoryHeaderProcessorComponent extends HistoryApi {

  val headerProcessor: HeaderProcessor

  trait HeaderProcessor {
    def processHeader(h: Header): ProgressInfo
  }
}
