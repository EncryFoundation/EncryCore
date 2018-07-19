package encry.view.history

import java.io.File

import encry.ModifierId
import encry.consensus.History
import encry.consensus.History.ProgressInfo
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.ADProofs
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.{EncryBlockHeader, EncryHeaderChain}
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.settings._
import encry.utils.NetworkTimeProvider
import encry.view.history.processors.payload.{BlockPayloadProcessor, EmptyBlockPayloadProcessor}
import encry.view.history.processors.proofs.{ADStateProofProcessor, FullStateProofProcessor}
import encry.view.history.storage.HistoryStorage
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}

import scala.util.Try

/**
  * History implementation. It is processing persistent modifiers generated locally or received from the network.
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
  *   2. Be ignored by history (verifyTransactions == false)
  */
trait EncryHistory extends EncryHistoryReader {

  def isFullBlockChainSynced: Boolean = bestHeaderOpt.exists(bestHeader => bestBlockOpt.exists(_.header.id == bestHeader.id))

  /** Appends modifier to the history if it is applicable. */
  def append(modifier: EncryPersistentModifier): Try[(EncryHistory, History.ProgressInfo[EncryPersistentModifier])] = {
    log.info(s"Trying to append modifier ${Algos.encode(modifier.id)} of type ${modifier.modifierTypeId} to history")
    Try {
      modifier match {
        case header: EncryBlockHeader => (this, process(header))
        case payload: EncryBlockPayload => (this, process(payload))
        case adProofs: ADProofs => (this, process(adProofs))
      }
    }
  }

  def reportModifierIsValid(modifier: EncryPersistentModifier): EncryHistory = {
    log.info(s"Modifier ${modifier.encodedId} of type ${modifier.modifierTypeId} is marked as valid ")
    markModifierValid(modifier)
    this
  }

  /** Report some modifier as valid or invalid semantically */
  def reportModifierIsInvalid(modifier: EncryPersistentModifier, progressInfo: ProgressInfo[EncryPersistentModifier]):
  (EncryHistory, ProgressInfo[EncryPersistentModifier]) = {
    log.info(s"Modifier ${modifier.encodedId} of type ${modifier.modifierTypeId} is marked as invalid")
    this -> markModifierInvalid(modifier)
  }

  /** @return header, that corresponds to modifier */
  protected def correspondingHeader(modifier: EncryPersistentModifier): Option[EncryBlockHeader] = modifier match {
    case header: EncryBlockHeader => Some(header)
    case block: EncryBlock => Some(block.header)
    case proof: ADProofs => typedModifierById[EncryBlockHeader](proof.headerId)
    case payload: EncryBlockPayload => typedModifierById[EncryBlockHeader](payload.headerId)
    case _ => None
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
        val invalidatedHeaders: Seq[EncryBlockHeader] = continuationHeaderChains(invalidatedHeader, _ => true).flatten.distinct
        val validityRow: Seq[(ByteArrayWrapper, ByteArrayWrapper)] = invalidatedHeaders
          .flatMap(h => Seq(h.id, h.payloadId, h.adProofsId)
            .map(id => validityKey(id) -> ByteArrayWrapper(Array(0.toByte))))
        log.info(s"Going to invalidate ${invalidatedHeader.encodedId} and ${invalidatedHeaders.map(_.encodedId)}")
        val bestHeaderIsInvalidated: Boolean = bestHeaderIdOpt.exists(id => invalidatedHeaders.exists(_.id sameElements id))
        val bestFullIsInvalidated: Boolean = bestBlockIdOpt.exists(id => invalidatedHeaders.exists(_.id sameElements id))
        (bestHeaderIsInvalidated, bestFullIsInvalidated) match {
          case (false, false) =>
            // Modifiers from best header and best full chain are not involved, no rollback and links change required.
            historyStorage.insert(validityKey(modifier.id), validityRow)
            ProgressInfo[EncryPersistentModifier](None, Seq.empty, Seq.empty, Seq.empty)
          case _ =>
            // Modifiers from best header and best full chain are involved, links change required.
            val newBestHeader: EncryBlockHeader = loopHeightDown(bestHeaderHeight, id => !invalidatedHeaders.exists(_.id sameElements id))
              .ensuring(_.isDefined, "Where unable to find new best header, can't invalidate genesis block")
              .get

            if (!bestFullIsInvalidated) {
              // Only headers chain involved.
              historyStorage.insert(validityKey(modifier.id), Seq(BestHeaderKey -> ByteArrayWrapper(newBestHeader.id)))
              ProgressInfo[EncryPersistentModifier](None, Seq.empty, Seq.empty, Seq.empty)
            } else {
              val invalidatedChain: Seq[EncryBlock] = bestBlockOpt.toSeq
                .flatMap(f => headerChainBack(bestBlockHeight + 1, f.header, h => !invalidatedHeaders.contains(h)).headers)
                .flatMap(h => getBlock(h))
                .ensuring(_.lengthCompare(1) > 0, "invalidatedChain should contain at least bestFullBlock and parent")
              val branchPoint: EncryBlock = invalidatedChain.head
              val validChain: Seq[EncryBlock] =
                continuationHeaderChains(branchPoint.header, h => getBlock(h).isDefined && !invalidatedHeaders.contains(h))
                  .maxBy(chain => scoreOf(chain.last.id).getOrElse(BigInt(0)))
                  .flatMap(h => getBlock(h))
              val changedLinks: Seq[(ByteArrayWrapper, ByteArrayWrapper)] =
                Seq(BestBlockKey -> ByteArrayWrapper(validChain.last.id), BestHeaderKey -> ByteArrayWrapper(newBestHeader.id))
              val toInsert: Seq[(ByteArrayWrapper, ByteArrayWrapper)] = validityRow ++ changedLinks
              historyStorage.insert(validityKey(modifier.id), toInsert)
              ProgressInfo[EncryPersistentModifier](Some(branchPoint.id), invalidatedChain.tail, validChain.tail, Seq.empty)
            }
        }
      case None =>
        // No headers become invalid. Just mark this particular modifier as invalid.
        historyStorage.insert(validityKey(modifier.id), Seq(validityKey(modifier.id) -> ByteArrayWrapper(Array(0.toByte))))
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
      case block: EncryBlock =>
        val nonMarkedIds: Seq[ModifierId] = (Seq(block.header.id, block.payload.id) ++ block.adProofsOpt.map(_.id))
          .filter(id => historyStorage.get(validityKey(id)).isEmpty)
        if (nonMarkedIds.nonEmpty) historyStorage.
          insert(validityKey(nonMarkedIds.head), nonMarkedIds.map(id => validityKey(id) -> ByteArrayWrapper(Array(1.toByte))))
        if (bestBlockOpt.contains(block))
          ProgressInfo[EncryPersistentModifier](None, Seq.empty, Seq.empty, Seq.empty) // Applies best header to the history
        else {
          // Marks non-best full block as valid. Should have more blocks to apply to sync state and history.
          val bestFullHeader: EncryBlockHeader = bestBlockOpt.get.header
          val limit: Int = bestFullHeader.height - block.header.height
          val chainBack: EncryHeaderChain = headerChainBack(limit, bestFullHeader, h => h.parentId sameElements block.header.id)
            .ensuring(_.headOption.isDefined, s"Should have next block to apply, failed for ${block.header}")
          // Block in the best chain that is linked to this header.
          val toApply: Option[EncryBlock] = chainBack.headOption.flatMap(opt => getBlock(opt))
            .ensuring(_.isDefined, s"Should be able to get full block for header ${chainBack.headOption}")
            .ensuring(_.get.header.parentId sameElements block.header.id,
              s"Block to appy should link to current block. Failed for ${chainBack.headOption} and ${block.header}")
          ProgressInfo[EncryPersistentModifier](None, Seq.empty, toApply.toSeq, Seq.empty)
        }
      case _ =>
        historyStorage.insert(validityKey(modifier.id), Seq(validityKey(modifier.id) -> ByteArrayWrapper(Array(1.toByte))))
        ProgressInfo[EncryPersistentModifier](None, Seq.empty, Seq.empty, Seq.empty)
    }

  def closeStorage(): Unit = historyStorage.close()
}

object EncryHistory {

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

  def readOrGenerate(settings: EncryAppSettings, ntp: NetworkTimeProvider): EncryHistory = {

    val historyIndexDir: File = getHistoryIndexDir(settings)
    val historyObjectsDir: File = getHistoryObjectsDir(settings)

    val indexStore: LSMStore = new LSMStore(historyIndexDir, keepVersions = 0)
    val objectsStore: LSMStore = new LSMStore(historyObjectsDir, keepVersions = 0)
    val storage: HistoryStorage = new HistoryStorage(indexStore, objectsStore)

    val history: EncryHistory = (settings.node.stateMode.isDigest, settings.node.verifyTransactions) match {
      case (true, true) =>
        new EncryHistory with ADStateProofProcessor with BlockPayloadProcessor {
          override protected val nodeSettings: NodeSettings = settings.node
          override protected val historyStorage: HistoryStorage = storage
          override protected val timeProvider: NetworkTimeProvider = ntp
        }
      case (false, true) =>
        new EncryHistory with FullStateProofProcessor with BlockPayloadProcessor {
          override protected val nodeSettings: NodeSettings = settings.node
          override protected val historyStorage: HistoryStorage = storage
          override protected val timeProvider: NetworkTimeProvider = ntp
        }
      case (true, false) =>
        new EncryHistory with ADStateProofProcessor with EmptyBlockPayloadProcessor {
          override protected val nodeSettings: NodeSettings = settings.node
          override protected val historyStorage: HistoryStorage = storage
          override protected val timeProvider: NetworkTimeProvider = ntp
        }
      case m => throw new Error(s"Unsupported settings ADState=:${m._1}, verifyTransactions=:${m._2}, ")
    }
    history
  }
}
