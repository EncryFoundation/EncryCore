package encry.view.history

import java.io.File

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.ADProofs
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.settings._
import encry.view.history.processors.payload.{BlockPayloadProcessor, EmptyBlockPayloadProcessor}
import encry.view.history.processors.proofs.{ADStateProofProcessor, FullStateProofProcessor}
import encry.view.history.storage.{FileHistoryObjectsStore, HistoryStorage}
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import scorex.core.ModifierId
import scorex.core.consensus.History
import scorex.core.consensus.History.ProgressInfo
import scorex.crypto.encode.Base58

import scala.util.Try

/**
  * History implementation. It is processing persistent modifiers generated locally or coming from network.
  * Depending on chosen node settings, it will process modifiers in a different way, different processors define how to
  * process different type of modifiers.
  *
  * HeadersProcessor: processor of block headers. It's the same for all node settings
  * BlockTransactionsProcessor: Processor of BlockTransactions. BlockTransactions may
  *   1. Be downloaded from other peers (verifyTransactions == true)
  *   2. Be ignored by history (verifyTransactions == false)
  */
trait EncryHistory extends History[EncryPersistentModifier, EncrySyncInfo, EncryHistory]
  with EncryHistoryReader {

  // Appends modifier to the history if it is applicable.
  override def append(modifier: EncryPersistentModifier): Try[(EncryHistory, History.ProgressInfo[EncryPersistentModifier])] = {
    log.debug(s"Trying to append modifier ${Base58.encode(modifier.id)} of type ${modifier.modifierTypeId} to history...")
    testApplicable(modifier).map { _ =>
      modifier match {
        case header: EncryBlockHeader => (this, process(header))
        case payload: EncryBlockPayload => (this, process(payload))
        case adProofs: ADProofs => (this, process(adProofs))
      }
    }
  }

  /**
    * @param modifier
    * @return header, that corresponds to modifier
    */
  protected def correspondingHeader(modifier: EncryPersistentModifier): Option[EncryBlockHeader] = modifier match {
    case h: EncryBlockHeader => Some(h)
    case full: EncryBlock => Some(full.header)
    case proof: ADProofs => typedModifierById[EncryBlockHeader](proof.headerId)
    case txs: EncryBlockPayload => typedModifierById[EncryBlockHeader](txs.headerId)
    case _ => None
  }

  /**
    * Marks modifier and all modifiers in child chains as invalid
    *
    * @param modifier that is invalid from State point of view
    * @return ProgressInfo with next modifier to try to apply
    */
  private def markModifierInvalid(modifier: EncryPersistentModifier): ProgressInfo[EncryPersistentModifier] =
    correspondingHeader(modifier) match {
      case Some(invalidatedHeader) =>
        val invalidatedHeaders = continuationHeaderChains(invalidatedHeader, _ => true).flatten.distinct
        val validityRow = invalidatedHeaders.flatMap(h => Seq(h.id, h.payloadId, h.adProofsId)
          .map(id => validityKey(id) -> ByteArrayWrapper(Array(0.toByte))))
        log.info(s"Going to invalidate ${invalidatedHeader.encodedId} and ${invalidatedHeaders.map(_.encodedId)}")
        val bestHeaderIsInvalidated = bestHeaderIdOpt.exists(id => invalidatedHeaders.exists(_.id sameElements id))
        val bestFullIsInvalidated = bestFullBlockIdOpt.exists(id => invalidatedHeaders.exists(_.id sameElements id))
        (bestHeaderIsInvalidated, bestFullIsInvalidated) match {
          case (false, false) =>
            // Modifiers from best header and best full chain are not involved, no rollback and links change required.
            historyStorage.insert(validityKey(modifier.id), validityRow)
            ProgressInfo[EncryPersistentModifier](None, Seq.empty, None, Seq.empty)
          case _ =>
            // Modifiers from best header and best full chain are involved, links change required.
            val newBestHeader = loopHeightDown(bestHeaderHeight, id => !invalidatedHeaders.exists(_.id sameElements id))
              .ensuring(_.isDefined, "Where unable to find new best header, can't invalidate genesis block")
              .get

            if (!bestFullIsInvalidated) {
              // Only headers chain involved.
              historyStorage.insert(validityKey(modifier.id),
                Seq(BestHeaderKey -> ByteArrayWrapper(newBestHeader.id)))
              ProgressInfo[EncryPersistentModifier](None, Seq.empty, None, Seq.empty)
            } else {
              val invalidatedChain: Seq[EncryBlock] = bestFullBlockOpt.toSeq
                .flatMap(f => headerChainBack(bestFullBlockHeight + 1, f.header, h => !invalidatedHeaders.contains(h)).headers)
                .flatMap(h => getFullBlock(h))
                .ensuring(_.lengthCompare(1) > 0, "invalidatedChain should contain at least bestFullBlock and parent")

              val branchPoint = invalidatedChain.head
              val validChain: Seq[EncryBlock] = {
                continuationHeaderChains(branchPoint.header,
                  h => getFullBlock(h).isDefined && !invalidatedHeaders.contains(h))
                  .maxBy(chain => scoreOf(chain.last.id).getOrElse(BigInt(0)))
                  .flatMap(h => getFullBlock(h))
              }

              val changedLinks = Seq(BestFullBlockKey -> ByteArrayWrapper(validChain.last.id),
                BestHeaderKey -> ByteArrayWrapper(newBestHeader.id))
              val toInsert = validityRow ++ changedLinks
              historyStorage.insert(validityKey(modifier.id), toInsert)
              ProgressInfo[EncryPersistentModifier](Some(branchPoint.id), invalidatedChain.tail,
                validChain.tail.headOption, Seq.empty)
            }
        }
      case None =>
        // No headers become invalid. Just mark this particular modifier as invalid.
        historyStorage.insert(validityKey(modifier.id),
          Seq(validityKey(modifier.id) -> ByteArrayWrapper(Array(0.toByte))))
        ProgressInfo[EncryPersistentModifier](None, Seq.empty, None, Seq.empty)
    }

  /**
    * Marks modifier as valid
    *
    * @param modifier that is invalid from State point of view
    * @return ProgressInfo with next modifier to try to apply
    */
  private def markModifierValid(modifier: EncryPersistentModifier): ProgressInfo[EncryPersistentModifier] =
    modifier match {
      case block: EncryBlock =>
        val nonMarkedIds = (Seq(block.header.id, block.payload.id) ++ block.adProofsOpt.map(_.id))
          .filter(id => historyStorage.get(validityKey(id)).isEmpty)

        if (nonMarkedIds.nonEmpty) {
          historyStorage.insert(validityKey(nonMarkedIds.head),
            nonMarkedIds.map(id => validityKey(id) -> ByteArrayWrapper(Array(1.toByte))))
        }
        if (bestFullBlockOpt.contains(block)) {
          // Applies best header to the history
          ProgressInfo[EncryPersistentModifier](None, Seq.empty, None, Seq.empty)
        } else {
          // Marks non-best full block as valid. Should have more blocks to apply to sync state and history.
          val bestFullHeader = bestFullBlockOpt.get.header
          val limit = bestFullHeader.height - block.header.height
          val chainBack = headerChainBack(limit, bestFullHeader, h => h.parentId sameElements block.header.id)
            .ensuring(_.headOption.isDefined, s"Should have next block to apply, failed for ${block.header}")
          // Block in the best chain that is linked to this header.
          val toApply = chainBack.headOption.flatMap(opt => getFullBlock(opt))
            .ensuring(_.isDefined, s"Should be able to get full block for header ${chainBack.headOption}")
            .ensuring(_.get.header.parentId sameElements block.header.id,
              s"Block to appy should link to current block. Failed for ${chainBack.headOption} and ${block.header}")
          ProgressInfo[EncryPersistentModifier](None, Seq.empty, toApply, Seq.empty)
        }
      case _ =>
        historyStorage.insert(validityKey(modifier.id),
          Seq(validityKey(modifier.id) -> ByteArrayWrapper(Array(1.toByte))))
        ProgressInfo[EncryPersistentModifier](None, Seq.empty, None, Seq.empty)
    }

  override def reportSemanticValidity(modifier: EncryPersistentModifier, valid: Boolean,
                                      unusedParam: ModifierId): (EncryHistory, ProgressInfo[EncryPersistentModifier]) =
    if (valid) this -> markModifierValid(modifier) else this -> markModifierInvalid(modifier)
}

object EncryHistory {

  def getHistoryDir(settings: EncryAppSettings): File = new File(s"${settings.directory}/history")

  def readOrGenerate(settings: EncryAppSettings): EncryHistory = {

    val historyDir = getHistoryDir(settings)
    historyDir.mkdirs()

    val db = new LSMStore(historyDir, keepVersions = 0)
    val objectsStore = new FileHistoryObjectsStore(historyDir.getAbsolutePath)
    val storage = new HistoryStorage(db, objectsStore)

    val _nodeSettings = settings.nodeSettings

    val history: EncryHistory = (_nodeSettings.ADState, _nodeSettings.verifyTransactions) match {
      case (true, true) =>
        new EncryHistory with ADStateProofProcessor with BlockPayloadProcessor {
          override protected val chainSettings: ChainSettings = settings.chainSettings
          override protected val nodeSettings: NodeSettings = _nodeSettings
          override protected val historyStorage: HistoryStorage = storage
        }
      case (false, true) =>
        new EncryHistory with FullStateProofProcessor with BlockPayloadProcessor {
          override protected val chainSettings: ChainSettings = settings.chainSettings
          override protected val nodeSettings: NodeSettings = _nodeSettings
          override protected val historyStorage: HistoryStorage = storage
        }
      case (true, false) =>
        new EncryHistory with ADStateProofProcessor with EmptyBlockPayloadProcessor {
          override protected val chainSettings: ChainSettings = settings.chainSettings
          override protected val nodeSettings: NodeSettings = _nodeSettings
          override protected val historyStorage: HistoryStorage = storage
        }
      case m =>
        throw new Error(s"Unsupported settings combination ADState==${m._1}, verifyTransactions==${m._2}, ")
    }
    history
  }
}
