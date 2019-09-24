package encry.view.history

import java.io.File
import com.typesafe.scalalogging.StrictLogging
import encry.settings._
import encry.storage.VersionalStorage
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.storage.iodb.versionalIODB.IODBHistoryWrapper
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
import encry.utils.NetworkTimeProvider
import encry.view.history.storage.HistoryStorage
import io.iohk.iodb.LSMStore
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{Block, Header, Payload}
import org.encryfoundation.common.utils.TaggedTypes.{ModifierId, ModifierTypeId}
import org.iq80.leveldb.Options
import cats.syntax.either._
import cats.syntax.option._
import encry.consensus.HistoryConsensus.ProgressInfo
import encry.view.history.History.HistoryProcessingInfo
import encry.view.history.ValidationError.HistoryProcessingError

case class History(blockDownloadProcessor: BlockDownloadProcessor,
                   isHeaderChainSynced: Boolean,
                   timeProvider: NetworkTimeProvider,
                   storage: HistoryStorage)
  extends HistoryModifiersValidator with HistoryModifiersProcessors with AutoCloseable {

  def isFullChainSynced: Boolean = bestHeaderIdStorageApi
    .exists(bestHeaderId => bestBlockIdStorageApi.exists(_.sameElements(bestHeaderId)))

  def append(modifier: PersistentModifier): Either[HistoryProcessingError, (History, ProgressInfo)] =
    (modifier match {
      case header: Header   => processHeader(header)
      case payload: Payload => processPayload(payload)
    }) match {
      case Left(value) =>
        logger.info(s"Application failed for modifier ${modifier.encodedId} of type ${modifier.modifierTypeId} with error ${value.error}.")
        value.asLeft[(History, ProgressInfo)]
      case Right(HistoryProcessingInfo(blockDownloadingProcessor, bp, download, apply, remove, isHeaderChainSynced)) =>
        logger.info(s"Application finished successfully for modifier ${modifier.encodedId} of type ${modifier.modifierTypeId}.")
        (this.copy(blockDownloadingProcessor, isHeaderChainSynced), ProgressInfo(bp, remove, apply, download))
          .asRight[HistoryProcessingError]
    }

  private def correspondingHeader(modifier: PersistentModifier): Option[Header] = modifier match {
    case header: Header   => Some(header)
    case block: Block     => Some(block.header)
    case payload: Payload => headerByIdOpt(payload.headerId)
  }

  def reportModifierIsInvalid(modifier: PersistentModifier): ProgressInfo = {
    logger.info(s"Have been starting function reportModifierIsInvalid for modifier ${modifier.encodedId} of type ${modifier.modifierTypeId}.")
    correspondingHeader(modifier) match {
      case Some(invalidatedHeader) =>
        val invalidatedHeaders: Seq[Header] = continuationHeaderChains(invalidatedHeader, _ => true).flatten.distinct
        val validityRow: List[(StorageKey, StorageValue)] = invalidatedHeaders
          .flatMap(h =>
            Seq(h.id, h.payloadId).map(id => validityKey(id) -> StorageValue @@ Array(0.toByte))
          ).toList
        val bestHeaderIsInvalidated: Boolean = bestHeaderIdStorageApi.exists(id => invalidatedHeaders.exists(_.id sameElements id))
        val bestFullIsInvalidated: Boolean = bestBlockIdStorageApi.exists(id => invalidatedHeaders.exists(_.id sameElements id))
        (bestHeaderIsInvalidated, bestFullIsInvalidated) match {
          case (false, false) =>
            storage.insert(StorageVersion @@ validityKey(modifier.id).untag(StorageKey), validityRow)
            ProgressInfo.empty
          case _ =>
            val newBestHeader: Header =
              loopHeightDown(getBestHeaderHeight, id => !invalidatedHeaders.exists(_.id sameElements id))
                .ensuring(_.isDefined, "Where unable to find new best header, can't invalidate genesis block")
                .get
            if (!bestFullIsInvalidated) {
              storage.insert(
                StorageVersion @@ validityKey(modifier.id).untag(StorageKey),
                List(BestHeaderKey -> StorageValue @@ newBestHeader.id.untag(ModifierId))
              )
              ProgressInfo.empty
            } else {
              val invalidatedChain: Seq[Block] = bestBlockOpt.toSeq
                .flatMap(f => computeForkChain(getBestBlockHeight + 1, f.header, h => !invalidatedHeaders.contains(h)))
                .flatMap(blockByHeaderOpt)
                .ensuring(_.lengthCompare(1) > 0, "invalidatedChain should contain at least bestFullBlock and parent")
              val branchPoint: Block = invalidatedChain.head
              val validChain: Seq[Block] =
                continuationHeaderChains(branchPoint.header, h => blockByHeaderOpt(h).isDefined && !invalidatedHeaders.contains(h))
                  .maxBy(chain => scoreOf(chain.last.id).getOrElse(BigInt(0)))
                  .flatMap(blockByHeaderOpt)
              val changedLinks: Seq[(StorageKey, StorageValue)] =
                List(
                  BestBlockKey -> StorageValue @@ validChain.last.id.untag(ModifierId),
                  BestHeaderKey -> StorageValue @@ newBestHeader.id.untag(ModifierId)
                )
              val toInsert: List[(StorageKey, StorageValue)] = validityRow ++ changedLinks
              storage.insert(StorageVersion @@ validityKey(modifier.id).untag(StorageKey), toInsert)
              ProgressInfo(
                branchPoint.id.some,
                invalidatedChain.drop(1).toList,
                validChain.drop(1).toList,
                none
              )
            }
        }
      case None =>
        logger.info(s"No headers become invalid. Just mark this particular modifier as invalid.")
        storage.insert(
          StorageVersion @@ validityKey(modifier.id).untag(StorageKey),
          List(validityKey(modifier.id) -> StorageValue @@ Array(0.toByte))
        )
        ProgressInfo.empty
    }
  }

  def reportModifierIsValid(modifier: PersistentModifier): Unit = {
    logger.info(s"Have been starting function reportModifierIsValid for modifier ${modifier.encodedId} of type ${modifier.modifierTypeId}.")
    modifier match {
      case block: Block =>
        List(block.header.id, block.payload.id).filter(id => storage.get(validityKey(id)).isEmpty) match {
          case Nil => ()
          case list@ ::(head, _) =>
            storage.insert(
              StorageVersion @@ validityKey(head).untag(StorageKey),
              list.map(validityKey(_) -> StorageValue @@ Array(1.toByte))
            )
        }
      case _ =>
        storage.insert(
          StorageVersion @@ validityKey(modifier.id).untag(StorageKey),
          List(validityKey(modifier.id) -> StorageValue @@ Array(1.toByte))
        )
    }
  }

  override def close(): Unit = storage.close()

  def closeStorage(): Unit = storage.close()
}

object History extends StrictLogging {

  final case class HistoryProcessingInfo(blockDownloadingProcessor: BlockDownloadProcessor,
                                         branchPoint: Option[ModifierId],
                                         modifiersToDownload: Option[(ModifierTypeId, ModifierId)],
                                         modifiersToApply: List[PersistentModifier],
                                         modifiersToRemove: List[PersistentModifier],
                                         isHeaderChainSynced: Boolean)

  object HistoryProcessingInfo {
    def apply(blockDownloadingProcessor: BlockDownloadProcessor,
              isHeaderChainSynced: Boolean): HistoryProcessingInfo =
      new HistoryProcessingInfo(
        blockDownloadingProcessor,
        none[ModifierId],
        none[(ModifierTypeId, ModifierId)],
        List.empty[PersistentModifier],
        List.empty[PersistentModifier],
        isHeaderChainSynced
      )

    def apply(blockDownloadingProcessor: BlockDownloadProcessor,
              modifiersToApply: List[PersistentModifier],
              isHeaderChainSynced: Boolean
             ): HistoryProcessingInfo =
      new HistoryProcessingInfo(
        blockDownloadingProcessor,
        none[ModifierId],
        none[(ModifierTypeId, ModifierId)],
        modifiersToApply,
        List.empty[PersistentModifier],
        isHeaderChainSynced
      )
  }

  def getHistoryIndexDir(settings: EncryAppSettings): File = {
    val dir: File = new File(s"${settings.directory}/history/index")
    dir.mkdirs()
    dir
  }

  def getHistoryObjectsDir(settings: EncryAppSettings): File = {
    val dir: File = new File(s"${settings.directory}/history/objects")
    dir.mkdirs()
    dir
  }

  def readOrGenerate(settings: EncryAppSettings, ntp: NetworkTimeProvider): History = {

    val historyIndexDir: File = getHistoryIndexDir(settings)
    //Check what kind of storage in settings:
    val vldbInit = settings.storage.history match {
      case VersionalStorage.IODB =>
        logger.info("Init history with iodb storage")
        val historyObjectsDir: File = getHistoryObjectsDir(settings)
        val indexStore: LSMStore = new LSMStore(historyIndexDir, keepVersions = 0)
        val objectsStore: LSMStore = new LSMStore(historyObjectsDir, keepVersions = 0)
        IODBHistoryWrapper(indexStore, objectsStore)
      case VersionalStorage.LevelDB =>
        logger.info("Init history with levelDB storage")
        val levelDBInit = LevelDbFactory.factory.open(historyIndexDir, new Options)
        VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB))
    }
    History(BlockDownloadProcessor(settings), isHeaderChainSynced = false, ntp, HistoryStorage(vldbInit))
  }
}