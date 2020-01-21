package encry.view.history

import encry.consensus.HistoryConsensus.ProgressInfo
import org.encryfoundation.common.modifiers.history.Payload
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
import scala.collection.immutable.HashSet

trait HistoryPayloadsProcessorComponent extends HistoryPrivateApi {

  val payloadProcessor: PayloadProcessor
  def payloadsIdsToDownload(howMany: Int, excluding: HashSet[ModifierId]): List[ModifierId]

  trait PayloadProcessor {
    def processPayload(payload: Payload): ProgressInfo
  }

}
