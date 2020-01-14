package encry.view.history

import cats.syntax.option.none
import encry.consensus.HistoryConsensus.ProgressInfo
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import org.encryfoundation.common.modifiers.history.Payload

trait FastSyncProcessor { historyApi: HistoryApi =>

  def processPayload(payload: Payload): ProgressInfo = {
    val startTime: Long = System.currentTimeMillis()
    getBlockByPayload(payload).foreach { block =>
      logger.info(s"processPayloadFastSync")
      historyStorage.bulkInsert(payload.id, Seq(BestBlockKey -> payload.headerId), Seq(payload))
      blockDownloadProcessor.updateBestBlock(block.header)
      logger.info(s"BlockDownloadProcessor updated block at height ${block.header.height} successfully")
      historyStorage.insert(
        StorageVersion @@ validityKey(block.payload.id).untag(StorageKey),
        List(block.header.id, block.payload.id).map(id => validityKey(id) -> StorageValue @@ Array(1.toByte))
      )
      logger.info(s"Finished processing block ${block.encodedId}. " +
        s"Processing time is ${(System.currentTimeMillis() - startTime) / 1000} s")
    }
    ProgressInfo(none, Seq.empty, Seq.empty, none)
  }
}