package encry.view.history.tmp

import cats.syntax.either._
import encry.consensus.HistoryConsensus.ProgressInfo
import encry.settings.EncryAppSettings
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.view.history.History
import encry.view.history.storage.HistoryStorage
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{Block, Header, Payload}
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId

import scala.collection.immutable.HashSet

class History private () extends HistoryPrivateApi with AutoCloseable { this: HistoryPayloadsProcessorComponent =>

  var isFullChainSynced: Boolean

  def append(modifier: PersistentModifier): Either[Throwable, (History, ProgressInfo)] =
    Either.catchNonFatal(modifier match {
      case header: Header =>
        (this, processHeader(header))
      case payload: Payload => (this, processPayload(payload))
    })

  def processHeader(h: Header): ProgressInfo

  def processPayload(payload: Payload): ProgressInfo

  private def correspondingHeader(modifier: PersistentModifier): Option[Header] = modifier match {
    case header: Header   => Some(header)
    case block: Block     => Some(block.header)
    case payload: Payload => getHeaderById(payload.headerId)
  }

  def reportModifierIsInvalid(modifier: PersistentModifier): (History, ProgressInfo) =
    correspondingHeader(modifier) match {
      case Some(invalidatedHeader) =>
        val invalidatedHeaders: Seq[Header] = continuationHeaderChains(invalidatedHeader, _ => true).flatten.distinct
        val validityRow: List[(StorageKey, StorageValue)] = invalidatedHeaders
          .flatMap(
            h =>
              Seq(h.id, h.payloadId)
                .map(id => validityKey(id) -> StorageValue @@ Array(0.toByte))
          )
          .toList
        val bestHeaderIsInvalidated: Boolean =
          getBestHeaderId.exists(id => invalidatedHeaders.exists(_.id sameElements id))
        val bestFullIsInvalidated: Boolean =
          getBestBlockId.exists(id => invalidatedHeaders.exists(_.id sameElements id))
        (bestHeaderIsInvalidated, bestFullIsInvalidated) match {
          case (false, false) =>
            // Modifiers from best header and best full chain are not involved, no rollback and links change required.
            historyStorage.insert(StorageVersion @@ validityKey(modifier.id).untag(StorageKey), validityRow)
            this -> ProgressInfo(None, Seq.empty, Seq.empty, None)
          case _ =>
            // Modifiers from best header and best full chain are involved, links change required.
            val newBestHeader: Header =
              loopHeightDown(getBestHeaderHeight, id => !invalidatedHeaders.exists(_.id sameElements id))
                .ensuring(_.isDefined, "Where unable to find new best header, can't invalidate genesis block")
                .get

            if (!bestFullIsInvalidated) {
              // Only headers chain involved.
              historyStorage.insert(
                StorageVersion @@ validityKey(modifier.id).untag(StorageKey),
                List(BestHeaderKey -> StorageValue @@ newBestHeader.id.untag(ModifierId))
              )
              this -> ProgressInfo(None, Seq.empty, Seq.empty, None)
            } else {
              val invalidatedChain: Seq[Block] = getBestBlock.toSeq
                .flatMap(
                  f => headerChainBack(getBestBlockHeight + 1, f.header, h => !invalidatedHeaders.contains(h)).headers
                )
                .flatMap(h => getBlockByHeader(h))
                .ensuring(_.lengthCompare(1) > 0, "invalidatedChain should contain at least bestFullBlock and parent")
              val branchPoint: Block = invalidatedChain.head
              val validChain: Seq[Block] =
                continuationHeaderChains(branchPoint.header,
                                         h => getBlockByHeader(h).isDefined && !invalidatedHeaders.contains(h))
                  .maxBy(chain => scoreOf(chain.last.id).getOrElse(BigInt(0)))
                  .flatMap(h => getBlockByHeader(h))
              val changedLinks: Seq[(StorageKey, StorageValue)] =
                List(
                  BestBlockKey  -> StorageValue @@ validChain.last.id.untag(ModifierId),
                  BestHeaderKey -> StorageValue @@ newBestHeader.id.untag(ModifierId)
                )
              val toInsert: List[(StorageKey, StorageValue)] = validityRow ++ changedLinks
              historyStorage.insert(StorageVersion @@ validityKey(modifier.id).untag(StorageKey), toInsert)
              this -> ProgressInfo(Some(branchPoint.id), invalidatedChain.tail, validChain.tail, None)
            }
        }
      case None =>
        // No headers become invalid. Just mark this particular modifier as invalid.
        historyStorage.insert(
          StorageVersion @@ validityKey(modifier.id).untag(StorageKey),
          List(validityKey(modifier.id) -> StorageValue @@ Array(0.toByte))
        )
        this -> ProgressInfo(None, Seq.empty, Seq.empty, None)
    }

  def reportModifierIsValid(modifier: PersistentModifier): History =
    modifier match {
      case block: Block =>
        val nonMarkedIds: Seq[ModifierId] = Seq(block.header.id, block.payload.id)
          .filter(id => historyStorage.get(validityKey(id)).isEmpty)
        if (nonMarkedIds.nonEmpty)
          historyStorage.insert(
            StorageVersion @@ validityKey(nonMarkedIds.head).untag(StorageKey),
            nonMarkedIds.map(id => validityKey(id) -> StorageValue @@ Array(1.toByte)).toList
          )
        this
      case _ =>
        historyStorage.insert(
          StorageVersion @@ validityKey(modifier.id).untag(StorageKey),
          List(validityKey(modifier.id) -> StorageValue @@ Array(1.toByte))
        )
        this
    }

  override def close(): Unit = historyStorage.close()

  def closeStorage(): Unit = historyStorage.close()

  override def payloadsIdsToDownload(howMany: Int, excluding: HashSet[ModifierId]): List[ModifierId] = ???

  override protected[history] val historyStorage: HistoryStorage = _
  override protected[history] val settings: EncryAppSettings = _
  override val processor: History#PayloadProcessor = _
}

object History {}
