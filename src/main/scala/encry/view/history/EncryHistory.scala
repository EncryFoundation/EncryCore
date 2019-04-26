package encry.view.history

import java.io.File
import com.typesafe.scalalogging.StrictLogging
import encry.utils.CoreTaggedTypes.ModifierId
import encry.consensus.History.ProgressInfo
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history._
import encry.settings._
import encry.storage.VersionalStorage
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.storage.iodb.versionalIODB.IODBHistoryWrapper
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
import encry.utils.NetworkTimeProvider
import encry.view.history.processors.payload.{BlockPayloadProcessor, EmptyBlockPayloadProcessor}
import encry.view.history.processors.proofs.{ADStateProofProcessor, FullStateProofProcessor}
import encry.view.history.storage.HistoryStorage
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.encryfoundation.common.Algos
import org.iq80.leveldb.Options
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
trait EncryHistory extends EncryHistoryReader {

  def isFullChainSynced: Boolean = bestHeaderOpt
    .exists(bestHeader => bestBlockOpt.exists(b => ByteArrayWrapper(b.header.id) == ByteArrayWrapper(bestHeader.id)))

  /** Appends modifier to the history if it is applicable. */
  def append(modifier: EncryPersistentModifier): Try[(EncryHistory, ProgressInfo[EncryPersistentModifier])] = {
    logger.info(s"Trying to append modifier ${Algos.encode(modifier.id)} of type ${modifier.modifierTypeId} to history")
    Try(modifier match {
      case header: Header     => (this, process(header))
      case payload: Payload   => (this, process(payload))
      case adProofs: ADProofs => (this, process(adProofs))
    })
  }

  def reportModifierIsValid(modifier: EncryPersistentModifier): EncryHistory = {
    logger.info(s"Modifier ${modifier.encodedId} of type ${modifier.modifierTypeId} is marked as valid ")
    markModifierValid(modifier)
    this
  }

  /** Report some modifier as valid or invalid semantically */
  def reportModifierIsInvalid(modifier: EncryPersistentModifier, progressInfo: ProgressInfo[EncryPersistentModifier]):
  (EncryHistory, ProgressInfo[EncryPersistentModifier]) = {
    logger.info(s"Modifier ${modifier.encodedId} of type ${modifier.modifierTypeId} is marked as invalid")
    this -> markModifierInvalid(modifier)
  }

  /** @return header, that corresponds to modifier */
  protected def correspondingHeader(modifier: EncryPersistentModifier): Option[Header] = modifier match {
    case header: Header   => Some(header)
    case block: Block     => Some(block.header)
    case proof: ADProofs  => typedModifierById[Header](proof.headerId)
    case payload: Payload => typedModifierById[Header](payload.headerId)
    case _                => None
  }

  /**
    * Marks modifier and all modifiers in child chains as invalid
    *
    * @param modifier that is invalid against the State
    * @return ProgressInfo with next modifier to try to apply
    */
  private def markModifierInvalid(modifier: EncryPersistentModifier): ProgressInfo[EncryPersistentModifier] =
    correspondingHeader(modifier) match {
      case Some(invalidatedHeader) =>
        val invalidatedHeaders: Seq[Header] = continuationHeaderChains(invalidatedHeader, _ => true).flatten.distinct
        val validityRow: List[(StorageKey, StorageValue)] = invalidatedHeaders
          .flatMap(h => Seq(h.id, h.payloadId, h.adProofsId)
            .map(id => validityKey(id) -> StorageValue @@ Array(0.toByte))).toList
        logger.info(s"Going to invalidate ${invalidatedHeader.encodedId} and ${invalidatedHeaders.map(_.encodedId)}")
        val bestHeaderIsInvalidated: Boolean = bestHeaderIdOpt.exists(id => invalidatedHeaders.exists(_.id sameElements id))
        val bestFullIsInvalidated: Boolean = bestBlockIdOpt.exists(id => invalidatedHeaders.exists(_.id sameElements id))
        (bestHeaderIsInvalidated, bestFullIsInvalidated) match {
          case (false, false) =>
            // Modifiers from best header and best full chain are not involved, no rollback and links change required.
            historyStorage.insert(StorageVersion @@ validityKey(modifier.id).untag(StorageKey), validityRow)
            ProgressInfo[EncryPersistentModifier](None, Seq.empty, Seq.empty, Seq.empty)
          case _ =>
            // Modifiers from best header and best full chain are involved, links change required.
            val newBestHeader: Header = loopHeightDown(bestHeaderHeight, id => !invalidatedHeaders.exists(_.id sameElements id))
              .ensuring(_.isDefined, "Where unable to find new best header, can't invalidate genesis block").get

            if (!bestFullIsInvalidated) {
              // Only headers chain involved.
              historyStorage.insert(
                StorageVersion @@ validityKey(modifier.id).untag(StorageKey),
                List(BestHeaderKey -> StorageValue @@ newBestHeader.id.untag(ModifierId))
              )
              ProgressInfo[EncryPersistentModifier](None, Seq.empty, Seq.empty, Seq.empty)
            } else {
              val invalidatedChain: Seq[Block] = bestBlockOpt.toSeq
                .flatMap(f => headerChainBack(bestBlockHeight + 1, f.header, h => !invalidatedHeaders.contains(h)).headers)
                .flatMap(h => getBlock(h))
                .ensuring(_.lengthCompare(1) > 0, "invalidatedChain should contain at least bestFullBlock and parent")
              val branchPoint: Block = invalidatedChain.head
              val validChain: Seq[Block] =
                continuationHeaderChains(branchPoint.header, h => getBlock(h).isDefined && !invalidatedHeaders.contains(h))
                  .maxBy(chain => scoreOf(chain.last.id).getOrElse(BigInt(0)))
                  .flatMap(h => getBlock(h))
              val changedLinks: Seq[(StorageKey, StorageValue)] =
                List(
                  BestBlockKey  -> StorageValue @@ validChain.last.id.untag(ModifierId),
                  BestHeaderKey -> StorageValue @@ newBestHeader.id.untag(ModifierId)
                )
              val toInsert: List[(StorageKey, StorageValue)] = validityRow ++ changedLinks
              historyStorage.insert(StorageVersion @@ validityKey(modifier.id).untag(StorageKey), toInsert)
              ProgressInfo[EncryPersistentModifier](Some(branchPoint.id), invalidatedChain.tail, validChain.tail, Seq.empty)
            }
        }
      case None =>
        // No headers become invalid. Just mark this particular modifier as invalid.
        historyStorage.insert(
          StorageVersion @@ validityKey(modifier.id).untag(StorageKey),
          List(validityKey(modifier.id) -> StorageValue @@ Array(0.toByte))
        )
        ProgressInfo[EncryPersistentModifier](None, Seq.empty, Seq.empty, Seq.empty)
    }

  /**
    * Marks modifier as valid
    *
    * @param modifier that is valid against the State
    * @return ProgressInfo with next modifier to try to apply
    */
  private def markModifierValid(modifier: EncryPersistentModifier): ProgressInfo[EncryPersistentModifier] =
    modifier match {
      case block: Block =>
        val nonMarkedIds: Seq[ModifierId] = (Seq(block.header.id, block.payload.id) ++ block.adProofsOpt.map(_.id))
          .filter(id => historyStorage.get(validityKey(id)).isEmpty)
        if (nonMarkedIds.nonEmpty) {
          historyStorage.
            insert(
              StorageVersion @@ validityKey(nonMarkedIds.head).untag(StorageKey),
              nonMarkedIds.map(id => validityKey(id) -> StorageValue @@ Array(1.toByte)).toList
            )
        }
        if (bestBlockOpt.contains(block))
          ProgressInfo[EncryPersistentModifier](None, Seq.empty, Seq.empty, Seq.empty) // Applies best header to the history
        else {
          // Marks non-best full block as valid. Should have more blocks to apply to sync state and history.
          val bestFullHeader: Header = bestBlockOpt.get.header
          val limit: Int = bestFullHeader.height - block.header.height
          val chainBack: HeaderChain = headerChainBack(limit, bestFullHeader, h => h.parentId sameElements block.header.id)
            .ensuring(_.headOption.isDefined, s"Should have next block to apply, failed for ${block.header}")
          // Block in the best chain that is linked to this header.
          logger.info(s"$block")
          logger.info(s"${chainBack.headOption}")
          val toApply: Option[Block] = chainBack.headOption.flatMap(opt => getBlock(opt))
            .ensuring(_.isDefined, s"Should be able to get full block for header ${chainBack.headOption}")
            .ensuring(_.get.header.parentId sameElements block.header.id,
              s"Block to apply should link to current block. Failed for ${chainBack.headOption} and ${block.header}")
          ProgressInfo[EncryPersistentModifier](None, Seq.empty, toApply.toSeq, Seq.empty)
        }
      case mod =>
        historyStorage.insert(
          StorageVersion @@ validityKey(modifier.id).untag(StorageKey),
          List(validityKey(modifier.id) -> StorageValue @@ Array(1.toByte))
        )
        ProgressInfo[EncryPersistentModifier](None, Seq.empty, Seq.empty, Seq.empty)
    }

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
    val storage: HistoryStorage = new HistoryStorage(vldbInit)

    val history: EncryHistory = (settingsEncry.node.stateMode.isDigest, settingsEncry.node.verifyTransactions) match {
      case (true, true) =>
        new EncryHistory with ADStateProofProcessor with BlockPayloadProcessor {
          override protected val settings: EncryAppSettings = settingsEncry
          override protected val nodeSettings: NodeSettings = settings.node
          override protected val historyStorage: HistoryStorage = storage
          override protected val timeProvider: NetworkTimeProvider = ntp
        }
      case (false, true) =>
        new EncryHistory with FullStateProofProcessor with BlockPayloadProcessor {
          override protected val settings: EncryAppSettings = settingsEncry
          override protected val nodeSettings: NodeSettings = settings.node
          override protected val historyStorage: HistoryStorage = storage
          override protected val timeProvider: NetworkTimeProvider = ntp
        }
      case (true, false) =>
        new EncryHistory with ADStateProofProcessor with EmptyBlockPayloadProcessor {
          override protected val settings: EncryAppSettings = settingsEncry
          override protected val nodeSettings: NodeSettings = settings.node
          override protected val historyStorage: HistoryStorage = storage
          override protected val timeProvider: NetworkTimeProvider = ntp
        }
      case m => throw new Error(s"Unsupported settings ADState=:${m._1}, verifyTransactions=:${m._2}, ")
    }
    history
  }
}
