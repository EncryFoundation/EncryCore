package encry.view.history.processors

import encry.consensus.HistoryConsensus.ProgressInfo
import org.encryfoundation.common.modifiers.history.Payload
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
import scala.collection.immutable.HashSet

trait HistoryPayloadProcessorComponent {

  val payloadProcessor: PayloadProcessor

  def payloadsIdsToDownload(howMany: Int, excluding: HashSet[ModifierId]): Seq[ModifierId]

  trait PayloadProcessor {
    def processPayload(payload: Payload): ProgressInfo
  }
}
