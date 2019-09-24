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
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{Block, Header, Payload}
import org.encryfoundation.common.utils.TaggedTypes.{ModifierId, ModifierTypeId}
import org.iq80.leveldb.Options
import cats.syntax.either._
import cats.syntax.option._
import encry.view.history.History.HistoryProcessingInfo
import encry.view.history.ValidationError.HistoryProcessingError

case class History(blockDownloadProcessor: BlockDownloadProcessor,
                   isHeaderChainSynced: Boolean,
                   timeProvider: NetworkTimeProvider,
                   storage: HistoryStorage)
  extends HistoryModifiersValidator with HistoryModifiersProcessors with AutoCloseable {

  def isFullChainSynced: Boolean = bestHeaderIdStorageApi
    .exists(bestHeaderId => bestBlockIdStorageApi.exists(bId => ByteArrayWrapper(bId) == ByteArrayWrapper(bestHeaderId)))

  def append(modifier: PersistentModifier): Either[HistoryProcessingError, (History, HistoryProcessingInfo)] =
    (modifier match {
      case header: Header => processHeader(header)
      case payload: Payload => processPayload(payload)
    }) match {
      case Left(value) => value.asLeft[(History, HistoryProcessingInfo)]
      case Right(h@HistoryProcessingInfo(blockDownloadingProcessor, _, _, _, _, isHeaderChainSynced)) =>
        (this.copy(blockDownloadingProcessor, isHeaderChainSynced), h).asRight[HistoryProcessingError]
    }

  /** @return header, that corresponds to modifier */
  private def correspondingHeader(modifier: PersistentModifier): Option[Header] = modifier match {
    case header: Header => Some(header)
    case block: Block => Some(block.header)
    case payload: Payload => headerByIdOpt(payload.headerId)
  }

  /**
    * Marks modifier and all modifiers in child chains as invalid
    *
    * @param modifier that is invalid against the State
    * @return ProgressInfo with next modifier to try to apply
    */
  def reportModifierIsInvalid(modifier: PersistentModifier): (History, HistoryProcessingInfo) = {
    logger.info(s"Modifier ${modifier.encodedId} of type ${modifier.modifierTypeId} is marked as invalid")
    correspondingHeader(modifier) match {
      case Some(invalidatedHeader) =>
        val invalidatedHeaders: Seq[Header] = continuationHeaderChains(invalidatedHeader, _ => true).flatten.distinct
        val validityRow: List[(StorageKey, StorageValue)] = invalidatedHeaders
          .flatMap(h => Seq(h.id, h.payloadId)
            .map(id => validityKey(id) -> StorageValue @@ Array(0.toByte))).toList
        logger.info(s"Going to invalidate ${invalidatedHeader.encodedId} and ${invalidatedHeaders.map(_.encodedId)}")
        val bestHeaderIsInvalidated: Boolean = bestHeaderIdStorageApi.exists(id => invalidatedHeaders.exists(_.id sameElements id))
        val bestFullIsInvalidated: Boolean = bestBlockIdStorageApi.exists(id => invalidatedHeaders.exists(_.id sameElements id))
        (bestHeaderIsInvalidated, bestFullIsInvalidated) match {
          case (false, false) =>
            // Modifiers from best header and best full chain are not involved, no rollback and links change required.
            storage.insert(StorageVersion @@ validityKey(modifier.id).untag(StorageKey), validityRow)
            this -> HistoryProcessingInfo(blockDownloadProcessor, isHeaderChainSynced)
          case _ =>
            // Modifiers from best header and best full chain are involved, links change required.
            val newBestHeader: Header =
              loopHeightDown(getBestHeaderHeight, id => !invalidatedHeaders.exists(_.id sameElements id))
                .ensuring(_.isDefined, "Where unable to find new best header, can't invalidate genesis block")
                .get

            if (!bestFullIsInvalidated) {
              // Only headers chain involved.
              storage.insert(
                StorageVersion @@ validityKey(modifier.id).untag(StorageKey),
                List(BestHeaderKey -> StorageValue @@ newBestHeader.id.untag(ModifierId))
              )
              this -> HistoryProcessingInfo(blockDownloadProcessor, isHeaderChainSynced)
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
              this -> HistoryProcessingInfo(
                blockDownloadProcessor,
                branchPoint.id.some,
                none,
                validChain.drop(1).toList,
                invalidatedChain.drop(1).toList,
                isHeaderChainSynced
              )
            }
        }
      case None =>
        logger.info(s"No headers become invalid. Just mark this particular modifier as invalid.")
        storage.insert(
          StorageVersion @@ validityKey(modifier.id).untag(StorageKey),
          List(validityKey(modifier.id) -> StorageValue @@ Array(0.toByte))
        )
        this -> HistoryProcessingInfo(blockDownloadProcessor, isHeaderChainSynced)
    }
  }

  def reportModifierIsValid(modifier: PersistentModifier): History = modifier match {
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

  def readOrGenerate(settingsEncry: EncryAppSettings, ntp: NetworkTimeProvider): History = {

    val historyIndexDir: File = getHistoryIndexDir(settingsEncry)
    //Check what kind of storage in settings:
    val vldbInit = settingsEncry.storage.history match {
      case VersionalStorage.IODB =>
        logger.info("Init history with iodb storage")
        val historyObjectsDir: File = getHistoryObjectsDir(settingsEncry)
        val indexStore: LSMStore = new LSMStore(historyIndexDir, keepVersions = 0)
        val objectsStore: LSMStore = new LSMStore(historyObjectsDir, keepVersions = 0)
        IODBHistoryWrapper(indexStore, objectsStore)
      case VersionalStorage.LevelDB =>
        logger.info("Init history with levelDB storage")
        val levelDBInit = LevelDbFactory.factory.open(historyIndexDir, new Options)
        VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settingsEncry.levelDB))
    }
    new History {
      override val storage: HistoryStorage = HistoryStorage(vldbInit)
      override val timeProvider: NetworkTimeProvider = new NetworkTimeProvider(settingsEncry.ntp)
    }
  }
}