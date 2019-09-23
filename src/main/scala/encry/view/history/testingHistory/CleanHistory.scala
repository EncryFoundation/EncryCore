package encry.view.history.testingHistory

import cats.syntax.option._
import cats.syntax.either._
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.utils.NetworkTimeProvider
import encry.view.history.storage.HistoryStorage
import encry.view.history.testingHistory.CleanHistory.HistoryProcessingInfo
import encry.view.history.testingHistory.HistoryErrors.HistoryProcessingError
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{Block, Header, Payload}
import org.encryfoundation.common.utils.TaggedTypes.{ModifierId, ModifierTypeId}

case class CleanHistory(blockDownloadingProcessor: CleanBlockDownloadingProcessor,
                        isHeaderChainSynced: Boolean,
                        timeProvider: NetworkTimeProvider,
                        storage: HistoryStorage) extends CleanHistoryModifiersProcessor {

  def append(modifier: PersistentModifier): Either[HistoryProcessingError, (CleanHistory, HistoryProcessingInfo)] =
    (modifier match {
      case header: Header => processHeader(header)
      case payload: Payload => processPayload(payload)
    }) match {
      case Left(value) => value.asLeft[(CleanHistory, HistoryProcessingInfo)]
      case Right(h@HistoryProcessingInfo(blockDownloadingProcessor, _, _, _, _, isHeaderChainSynced)) =>
        (this.copy(blockDownloadingProcessor, isHeaderChainSynced), h).asRight[HistoryProcessingError]
    }

  def reportModifierIsValid(modifier: PersistentModifier): CleanHistory =
    modifier match {
      case block: Block =>
        List(block.header.id, block.payload.id).filter(id => storage.get(validityKey(id)).isEmpty) match {
          case Nil => this
          case list@ ::(head, _) => storage.insert(
            StorageVersion @@ validityKey(head).untag(StorageKey),
            list.map(validityKey(_) -> StorageValue @@ Array(1.toByte)))
            this
        }
      case _ => storage.insert(
        StorageVersion @@ validityKey(modifier.id).untag(StorageKey),
        List(validityKey(modifier.id) -> StorageValue @@ Array(1.toByte)))
        this
    }

  def reportModifierIsInValid(modifier: PersistentModifier): Either[HistoryProcessingError, (CleanHistory, HistoryProcessingInfo)] =
    correspondingHeader(modifier) match {
      case Some(invalidatedHeader) =>
        val invalidatedHeaders: List[Header] = continuationHeaderChains(invalidatedHeader, _ => true).flatten.distinct
        val validityToInsert: List[(StorageKey, StorageValue)] = invalidatedHeaders.flatMap(h =>
          Seq(h.id, h.payloadId).map(id => validityKey(id) -> StorageValue @@ Array(0.toByte)))
        val bestHeaderIsInvalidated: Boolean = bestHeaderId.exists(id => invalidatedHeaders.exists(_.id sameElements id))
        val bestFullIsInvalidated: Boolean = bestBlockId.exists(id => invalidatedHeaders.exists(_.id sameElements id))
        (bestHeaderIsInvalidated, bestFullIsInvalidated) match {
          case (false, false) =>
            storage.insert(StorageVersion @@ validityKey(modifier.id).untag(StorageKey), validityToInsert)
            (this, HistoryProcessingInfo(blockDownloadingProcessor, isHeaderChainSynced)).asRight
          case _ if !bestFullIsInvalidated =>
            loopHeightDown(bestHeaderHeightStorageApi, id => !invalidatedHeaders.exists(_.id sameElements id)) match {
              case Some(newBestHeader) =>
                storage.insert(
                  StorageVersion @@ validityKey(modifier.id).untag(StorageKey),
                  List(BestHeaderKey -> StorageValue @@ newBestHeader.id.untag(ModifierId))
                )
                (this, HistoryProcessingInfo(blockDownloadingProcessor, isHeaderChainSynced)).asRight
              case None => HistoryProcessingError("Unable to find new best header, can't invalidate genesis block").asLeft
            }
          case _ =>
            loopHeightDown(bestHeaderHeightStorageApi, id => !invalidatedHeaders.exists(_.id sameElements id)) match {
              case Some(newBestHeader) =>
                val invalidatedChain: List[Block] = bestBlockStorageApi.toList
                  .flatMap(f => computeForkChain(bestBlockHeightStorageApi + 1, f.header, h => !invalidatedHeaders.contains(h)))
                  .flatMap(h => blockByHeaderStorageApi(h))
                  .ensuring(_.lengthCompare(1) > 0, "invalidatedChain should contain at least bestFullBlock and parent")
                val branchPoint: Block = invalidatedChain.head
                val validChain: List[Block] =
                  continuationHeaderChains(branchPoint.header, h => blockByHeaderStorageApi(h).isDefined && !invalidatedHeaders.contains(h))
                    .maxBy(chain => scoreOf(chain.last.id).getOrElse(BigInt(0)))
                    .flatMap(h => blockByHeaderStorageApi(h))
                val changedLinks: Seq[(StorageKey, StorageValue)] =
                  List(
                    BestBlockKey -> StorageValue @@ validChain.last.id.untag(ModifierId),
                    BestHeaderKey -> StorageValue @@ newBestHeader.id.untag(ModifierId)
                  )
                val toInsert: List[(StorageKey, StorageValue)] = validityToInsert ++ changedLinks
                storage.insert(StorageVersion @@ validityKey(modifier.id).untag(StorageKey), toInsert)
                (this, HistoryProcessingInfo(
                  blockDownloadingProcessor,
                  branchPoint.id.some,
                  none,
                  invalidatedChain.drop(1),
                  validChain.drop(1),
                  isHeaderChainSynced
                )).asRight
              case None => HistoryProcessingError("Unable to find new best header, can't invalidate genesis block").asLeft
            }
        }
      case None =>
        storage.insert(
          StorageVersion @@ validityKey(modifier.id).untag(StorageKey),
          List(validityKey(modifier.id) -> StorageValue @@ Array(0.toByte))
        )
        (this, HistoryProcessingInfo(blockDownloadingProcessor, isHeaderChainSynced)).asRight
    }

  //todo to the HistoryApi
  private def correspondingHeader(modifier: PersistentModifier): Option[Header] = modifier match {
    case header: Header => Some(header)
    case block: Block => Some(block.header)
    case payload: Payload => headerByIdStorageApi(payload.headerId)
  }

}

object CleanHistory {



}