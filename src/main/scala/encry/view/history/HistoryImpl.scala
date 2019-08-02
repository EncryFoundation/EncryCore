package encry.view.history

import java.io.File
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.History.ProgressInfo
import encry.modifiers.history._
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
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
import org.iq80.leveldb.Options
import cats.syntax.either._

/**
  * History implementation. It is processing persistent modifiers generated locally or received from the network.
  **/
trait HistoryImpl extends HistoryModifiersValidator with HistoryModifiersProcessors with AutoCloseable {

  def isFullChainSynced: Boolean = getBestHeaderId
    .exists(bestHeaderId => getBestBlockId.exists(bId => ByteArrayWrapper(bId) == ByteArrayWrapper(bestHeaderId)))

  /** Appends modifier to the history if it is applicable. */
  def append(modifier: PersistentModifier): Either[Throwable, (HistoryImpl, ProgressInfo)] = {
    logger.info(s"Trying to append modifier ${Algos.encode(modifier.id)} of type ${modifier.modifierTypeId} to history")
    Either.catchNonFatal(modifier match {
      case header: Header => (this, process(header))
      case payload: Payload => (this, process(payload))
    })
  }

  def reportModifierIsValid(modifier: PersistentModifier): HistoryImpl = {
    logger.info(s"Modifier ${modifier.encodedId} of type ${modifier.modifierTypeId} is marked as valid ")
    markModifierValid(modifier)
    this
  }

  /** Report some modifier as valid or invalid semantically */
  def reportModifierIsInvalid(modifier: PersistentModifier, progressInfo: ProgressInfo):
  (HistoryImpl, ProgressInfo) = {
    logger.info(s"Modifier ${modifier.encodedId} of type ${modifier.modifierTypeId} is marked as invalid")
    this -> markModifierInvalid(modifier)
  }

  /** @return header, that corresponds to modifier */
  protected def correspondingHeader(modifier: PersistentModifier): Option[Header] = modifier match {
    case header: Header => Some(header)
    case block: Block => Some(block.header)
    case payload: Payload => getHeaderById(payload.headerId)
    case _ => None
  }

  /**
    * Marks modifier and all modifiers in child chains as invalid
    *
    * @param modifier that is invalid against the State
    * @return ProgressInfo with next modifier to try to apply
    */
  private def markModifierInvalid(modifier: PersistentModifier): ProgressInfo = correspondingHeader(modifier) match {
    case Some(invalidatedHeader) =>
      val invalidatedHeaders: Seq[Header] = continuationHeaderChains(invalidatedHeader, _ => true).flatten.distinct
      val validityRow: List[(StorageKey, StorageValue)] = invalidatedHeaders
        .flatMap(h => Seq(h.id, h.payloadId)
          .map(id => validityKey(id) -> StorageValue @@ Array(0.toByte))).toList
      logger.info(s"Going to invalidate ${invalidatedHeader.encodedId} and ${invalidatedHeaders.map(_.encodedId)}")
      val bestHeaderIsInvalidated: Boolean = getBestHeaderId.exists(id => invalidatedHeaders.exists(_.id sameElements id))
      val bestFullIsInvalidated: Boolean = getBestBlockId.exists(id => invalidatedHeaders.exists(_.id sameElements id))
      (bestHeaderIsInvalidated, bestFullIsInvalidated) match {
        case (false, false) =>
          // Modifiers from best header and best full chain are not involved, no rollback and links change required.
          historyStorage.insert(StorageVersion @@ validityKey(modifier.id).untag(StorageKey), validityRow)
          ProgressInfo(None, Seq.empty, Seq.empty, None)
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
            ProgressInfo(None, Seq.empty, Seq.empty, None)
          } else {
            val invalidatedChain: Seq[Block] = getBestBlock.toSeq
              .flatMap(f => headerChainBack(getBestBlockHeight + 1, f.header, h => !invalidatedHeaders.contains(h)).headers)
              .flatMap(h => getBlockByHeader(h))
              .ensuring(_.lengthCompare(1) > 0, "invalidatedChain should contain at least bestFullBlock and parent")
            val branchPoint: Block = invalidatedChain.head
            val validChain: Seq[Block] =
              continuationHeaderChains(branchPoint.header, h => getBlockByHeader(h).isDefined && !invalidatedHeaders.contains(h))
                .maxBy(chain => scoreOf(chain.last.id).getOrElse(BigInt(0)))
                .flatMap(h => getBlockByHeader(h))
            val changedLinks: Seq[(StorageKey, StorageValue)] =
              List(
                BestBlockKey -> StorageValue @@ validChain.last.id.untag(ModifierId),
                BestHeaderKey -> StorageValue @@ newBestHeader.id.untag(ModifierId)
              )
            val toInsert: List[(StorageKey, StorageValue)] = validityRow ++ changedLinks
            historyStorage.insert(StorageVersion @@ validityKey(modifier.id).untag(StorageKey), toInsert)
            ProgressInfo(Some(branchPoint.id), invalidatedChain.tail, validChain.tail, None)
          }
      }
    case None =>
      // No headers become invalid. Just mark this particular modifier as invalid.
      historyStorage.insert(
        StorageVersion @@ validityKey(modifier.id).untag(StorageKey),
        List(validityKey(modifier.id) -> StorageValue @@ Array(0.toByte))
      )
      ProgressInfo(None, Seq.empty, Seq.empty, None)
  }

  /**
    * Marks modifier as valid
    *
    * @param modifier that is valid against the State
    * @return ProgressInfo with next modifier to try to apply
    */
  private def markModifierValid(modifier: PersistentModifier): ProgressInfo =
    modifier match {
      case block: Block =>
        val nonMarkedIds: Seq[ModifierId] = Seq(block.header.id, block.payload.id)
          .filter(id => historyStorage.get(validityKey(id)).isEmpty)
        if (nonMarkedIds.nonEmpty) {
          historyStorage.
            insert(
              StorageVersion @@ validityKey(nonMarkedIds.head).untag(StorageKey),
              nonMarkedIds.map(id => validityKey(id) -> StorageValue @@ Array(1.toByte)).toList
            )
        }
        if (getBestBlock.contains(block))
          ProgressInfo(None, Seq.empty, Seq.empty, None) // Applies best header to the history
        else {
          // Marks non-best full block as valid. Should have more blocks to apply to sync state and history.
          val bestFullHeader: Header = getBestBlock.get.header
          val limit: Int = bestFullHeader.height - block.header.height
          val chainBack: HeaderChain = headerChainBack(limit, bestFullHeader, h => h.parentId sameElements block.header.id)
            .ensuring(_.headOption.isDefined, s"Should have next block to apply, failed for ${block.header}")
          // Block in the best chain that is linked to this header.
          val toApply: Option[Block] = chainBack.headOption.flatMap(opt => getBlockByHeader(opt))
            .ensuring(_.isDefined, s"Should be able to get full block for header ${chainBack.headOption}")
            .ensuring(_.get.header.parentId sameElements block.header.id,
              s"Block to apply should link to current block. Failed for ${chainBack.headOption} and ${block.header}")
          ProgressInfo(None, Seq.empty, toApply.toSeq, None)
        }
      case mod =>
        historyStorage.insert(
          StorageVersion @@ validityKey(modifier.id).untag(StorageKey),
          List(validityKey(modifier.id) -> StorageValue @@ Array(1.toByte))
        )
        ProgressInfo(None, Seq.empty, Seq.empty, None)
    }

  override def close(): Unit = historyStorage.close()

  def closeStorage(): Unit = historyStorage.close()
}

object HistoryImpl extends StrictLogging {

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

  def readOrGenerate(settingsEncry: EncryAppSettings, ntp: NetworkTimeProvider): HistoryImpl = {

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
    new HistoryImpl {
      override val settings: EncryAppSettings = settingsEncry
      override val historyStorage: HistoryStorage = HistoryStorage(vldbInit)
      override val timeProvider: NetworkTimeProvider = new NetworkTimeProvider(settingsEncry.ntp)
    }
  }
}
