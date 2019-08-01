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
import encry.view.history.processors.payload.BlockPayloadProcessor
import encry.view.history.storage.HistoryStorage
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{Block, Header, Payload}
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
import org.iq80.leveldb.Options
import cats.syntax.either._

import scala.util.Try

/** History implementation. It is processing persistent modifiers generated locally or received from the network.
  * Depending on chosen node settings, it will process modifiers in a different way, different processors define how to
  * process different types of modifiers.
  *
  * HeadersProcessor: processor of block headers. It's the same for all node settings
  * ADProofsProcessor: processor of ADProofs. ADProofs may
  *   1. Be downloaded from other nodes (ADState == true)
  *   2. Be calculated by using local state (ADState == false)
  *   3. Be ignored by history in light mode (verifyTransactions == false)
  * BlockPayloadProcessor: Processor of BlockPayload. BlockPayload may
  *   1. Be downloaded from other peers (verifyTransactions == true)
  *   2. Be ignored by history (verifyTransactions == false) */
trait EncryHistory extends EncryHistoryReader with AutoCloseable {

  def isFullChainSynced: Boolean = getBestHeaderId
    .exists(bestHeaderId => getBestBlockId.exists(bId => ByteArrayWrapper(bId) == ByteArrayWrapper(bestHeaderId)))

  /** Appends modifier to the history if it is applicable. */
  def append(modifier: PersistentModifier): Either[Throwable, (EncryHistory, ProgressInfo[PersistentModifier])] = {
    logger.info(s"Trying to append modifier ${Algos.encode(modifier.id)} of type ${modifier.modifierTypeId} to history")
    Either.catchNonFatal(modifier match {
      case header: Header => (this, process(header))
      case payload: Payload => (this, process(payload))
    })
  }

  def reportModifierIsValid(modifier: PersistentModifier): EncryHistory = {
    logger.info(s"Modifier ${modifier.encodedId} of type ${modifier.modifierTypeId} is marked as valid ")
    markModifierValid(modifier)
    this
  }

  /** Report some modifier as valid or invalid semantically */
  def reportModifierIsInvalid(modifier: PersistentModifier, progressInfo: ProgressInfo[PersistentModifier]):
  (EncryHistory, ProgressInfo[PersistentModifier]) = {
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
  private def markModifierInvalid(modifier: PersistentModifier): ProgressInfo[PersistentModifier] =
    correspondingHeader(modifier) match {
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
            ProgressInfo[PersistentModifier](None, Seq.empty, Seq.empty, Seq.empty)
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
              ProgressInfo[PersistentModifier](None, Seq.empty, Seq.empty, Seq.empty)
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
              ProgressInfo[PersistentModifier](Some(branchPoint.id), invalidatedChain.tail, validChain.tail, Seq.empty)
            }
        }
      case None =>
        // No headers become invalid. Just mark this particular modifier as invalid.
        historyStorage.insert(
          StorageVersion @@ validityKey(modifier.id).untag(StorageKey),
          List(validityKey(modifier.id) -> StorageValue @@ Array(0.toByte))
        )
        ProgressInfo[PersistentModifier](None, Seq.empty, Seq.empty, Seq.empty)
    }

  /**
    * Marks modifier as valid
    *
    * @param modifier that is valid against the State
    * @return ProgressInfo with next modifier to try to apply
    */
  private def markModifierValid(modifier: PersistentModifier): ProgressInfo[PersistentModifier] =
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
          ProgressInfo[PersistentModifier](None, Seq.empty, Seq.empty, Seq.empty) // Applies best header to the history
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
          ProgressInfo[PersistentModifier](None, Seq.empty, toApply.toSeq, Seq.empty)
        }
      case mod =>
        historyStorage.insert(
          StorageVersion @@ validityKey(modifier.id).untag(StorageKey),
          List(validityKey(modifier.id) -> StorageValue @@ Array(1.toByte))
        )
        ProgressInfo[PersistentModifier](None, Seq.empty, Seq.empty, Seq.empty)
    }

  override def close(): Unit = historyStorage.close()

  def closeStorage(): Unit = historyStorage.close()
}

object EncryHistory extends StrictLogging {

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

  def readOrGenerate(settingsEncry: EncryAppSettings, ntp: NetworkTimeProvider): EncryHistory = {

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
    logger.info(s"Creating storage")
    val storage: HistoryStorage = new HistoryStorage(vldbInit)
    logger.info(s"Creating history")
    val history: EncryHistory = new EncryHistory with BlockPayloadProcessor {
      override val settings: EncryAppSettings = settingsEncry
      override val historyStorage: HistoryStorage = storage
      override protected val nodeSettings: NodeSettings = settingsEncry.node
      override val timeProvider: NetworkTimeProvider = new NetworkTimeProvider(settingsEncry.ntp)
    }
    logger.info(s"History created")
    history
  }
}
