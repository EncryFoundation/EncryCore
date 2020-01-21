package encry.view.history

import encry.consensus.HistoryConsensus.ProgressInfo
import org.encryfoundation.common.modifiers.history.Header

trait HistoryHeadersProcessorComponent extends HistoryPrivateApi {

  val headersProcessor: HeadersProcessor

  trait HeadersProcessor {
    def processHeader(h: Header): ProgressInfo
  }

}
