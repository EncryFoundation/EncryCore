package encry.view.history.processors

import encry.consensus.HistoryConsensus.ProgressInfo
import org.encryfoundation.common.modifiers.history.Header

trait HistoryHeaderProcessorComponent {

  val headerProcessor: HeaderProcessor

  trait HeaderProcessor {
    def processHeader(h: Header): ProgressInfo
  }
}
