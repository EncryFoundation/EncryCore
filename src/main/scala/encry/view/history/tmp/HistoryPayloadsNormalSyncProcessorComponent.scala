package encry.view.history.tmp

import encry.consensus.HistoryConsensus
import org.encryfoundation.common.modifiers.history.Payload
import org.encryfoundation.common.utils.TaggedTypes.ModifierId

import scala.collection.immutable.HashSet

trait HistoryPayloadsNormalSyncProcessorComponent extends HistoryPayloadsProcessorComponent {

  override val processor: PayloadProcessor = new NormalSyncProcessor

  class NormalSyncProcessor extends PayloadProcessor {
    override def processPayload(payload: Payload): HistoryConsensus.ProgressInfo = ???

    override def payloadsIdsToDownload(howMany: Int, excluding: HashSet[ModifierId]): List[ModifierId] = ???
  }
}
