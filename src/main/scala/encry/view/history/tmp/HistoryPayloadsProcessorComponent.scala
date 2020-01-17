package encry.view.history.tmp

import encry.consensus.HistoryConsensus.ProgressInfo
import org.encryfoundation.common.modifiers.history.Payload
import org.encryfoundation.common.utils.TaggedTypes.ModifierId

import scala.collection.immutable.HashSet

trait HistoryPayloadsProcessorComponent {

  val processor: PayloadProcessor

  trait PayloadProcessor {
    def processPayload(payload: Payload): ProgressInfo
    def payloadsIdsToDownload(howMany: Int, excluding: HashSet[ModifierId]): List[ModifierId]
  }

}
