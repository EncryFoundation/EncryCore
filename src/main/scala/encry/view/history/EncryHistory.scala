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
import io.iohk.iodb.{ByteArrayWrapper, LSMStore, Store}
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

  override def reportSemanticValidity(modifier: EncryPersistentModifier,
                                      valid: Boolean,
                                      unusedParam: ModifierId): (EncryHistory, ProgressInfo[EncryPersistentModifier]) = {
    def validityRowsForHeader(h: EncryBlockHeader): Seq[(ByteArrayWrapper, ByteArrayWrapper)] = {
      Seq(h.id, h.payloadId, h.adProofsId).map(id => validityKey(id) -> ByteArrayWrapper(Array(0.toByte)))
    }

    if (valid) {
      modifier match {
        case fb: EncryBlock =>
          val bestHeader = bestHeaderOpt.get
          val nonMarkedIds = (Seq(fb.header.id, fb.payload.id) ++ fb.adProofsOpt.map(_.id))
            .filter(id => historyStorage.db.get(validityKey(id)).isEmpty)

          if (nonMarkedIds.nonEmpty) {
            historyStorage.db.update(validityKey(nonMarkedIds.head), Seq(),
              nonMarkedIds.map(id => validityKey(id) -> ByteArrayWrapper(Array(1.toByte))))
          }

          val bestFull = bestFullBlockOpt.get
          if (fb == bestFull) {
            //applied best header to history
            this -> ProgressInfo[EncryPersistentModifier](None, Seq(), None, Seq())
          } else {
            //in fork processing
            val modHeight = heightOf(fb.header.id).get
            val chainBack = headerChainBack(bestHeaderHeight - modHeight, bestHeader, h => h.parentId sameElements fb.header.id)
            //block in the best chain that link to this header
            val toApply = chainBack.headOption.flatMap(opt => getFullBlock(opt))
            assert(toApply.get.header.parentId sameElements fb.header.id, "Should never be here, State is inconsistent")
            this -> ProgressInfo[EncryPersistentModifier](None, Seq(), toApply, Seq())
          }
        case _ =>
          historyStorage.db.update(validityKey(modifier.id), Seq(), Seq(validityKey(modifier.id) ->
            ByteArrayWrapper(Array(1.toByte))))
          this -> ProgressInfo[EncryPersistentModifier](None, Seq(), None, Seq())
      }

    } else {
      val headerOpt: Option[EncryBlockHeader] = modifier match {
        case h: EncryBlockHeader => Some(h)
        case full: EncryBlock => Some(full.header)
        case proof: ADProofs => typedModifierById[EncryBlockHeader](proof.headerId)
        case txs: EncryBlockPayload => typedModifierById[EncryBlockHeader](txs.headerId)
        case _ => None
      }
      headerOpt match {
        case Some(h) =>
          val invalidatedHeaders = continuationHeaderChains(h).flatMap(_.headers).distinct
          log.info(s"Invalidated header ${h.encodedId} and linked ${invalidatedHeaders.map(_.encodedId).mkString(",")}")
          val validityRow = invalidatedHeaders.flatMap(h => validityRowsForHeader(h))

          def isStillValid(id: ModifierId): Boolean = !invalidatedHeaders.exists(_.id sameElements id)

          def loopHeightDown(height: Int): EncryBlockHeader = {
            assert(height >= 0, s"Mark genesis invalid is not true")
            headerIdsAtHeight(height).find(id => isStillValid(id)).flatMap(id => typedModifierById[EncryBlockHeader](id)) match {
              case Some(header) => header
              case None => loopHeightDown(height - 1)
            }
          }

          val branchValidHeader: EncryBlockHeader = loopHeightDown(bestHeaderHeight)
          val bestValidFullOpt: Option[EncryBlockHeader] = bestFullBlockOpt.flatMap(h => heightOf(h.header.id))
            .map(loopHeightDown)

          if (bestHeaderOpt.contains(branchValidHeader) && bestFullBlockOpt.forall(b => bestValidFullOpt.contains(b.header))) {
            historyStorage.db.update(validityKey(modifier.id), Seq(), validityRow)
            this -> ProgressInfo[EncryPersistentModifier](None, Seq(), None, Seq())
          } else {
            val changedLinks = bestValidFullOpt.toSeq.map(h => BestFullBlockKey -> ByteArrayWrapper(h.id)) :+
              (BestHeaderKey, ByteArrayWrapper(branchValidHeader.id))
            val (validChain, invalidatedChain) = (bestValidFullOpt, bestFullBlockOpt) match {
              case (Some(bestValid), Some(bestFull)) =>
                val headersChain = commonBlockThenSuffixes(bestValid, bestFull.header)
                (headersChain._1.headers.flatMap(h => getFullBlock(h)), headersChain._2.headers.flatMap(h => getFullBlock(h)))
              case _ =>
                val headersChain = commonBlockThenSuffixes(branchValidHeader, bestHeaderOpt.get)
                (headersChain._1.headers, headersChain._2.headers)
            }
            assert(invalidatedChain.head == validChain.head, s"${invalidatedChain.head} == ${validChain.head}")
            val branchPoint: Some[ModifierId] = invalidatedChain.head match {
              case fullBlock: EncryBlock => Some(fullBlock.header.id)
              case header: EncryBlockHeader => Some(header.id)
            }

            val toInsert = validityRow ++ changedLinks
            historyStorage.db.update(validityKey(modifier.id), Seq(), toInsert)

            //TODO ???
            this -> ProgressInfo[EncryPersistentModifier](branchPoint, invalidatedChain.tail,
              validChain.tail.headOption, Seq())
          }
        case None =>
          historyStorage.db.update(validityKey(modifier.id), Seq(), Seq(validityKey(modifier.id) ->
            ByteArrayWrapper(Array(0.toByte))))
          this -> ProgressInfo[EncryPersistentModifier](None, Seq(), None, Seq())
      }
    }
  }

}

object EncryHistory {

  def readOrGenerate(settings: EncryAppSettings): EncryHistory = {
    val historyDir = new File(s"${settings.directory}/history")
    historyDir.mkdirs()

    val db = new LSMStore(historyDir, keepVersions = 0)

    val _nodeSettings = settings.nodeSettings

    // TODO: Processors mix-ins for `EncryHistory` at instantiating.
    val history: EncryHistory = (_nodeSettings.ADState, _nodeSettings.verifyTransactions) match {
      case (true, true) =>
        new EncryHistory with ADStateProofProcessor with BlockPayloadProcessor {
          override protected val chainSettings: ChainSettings = settings.chainSettings
          override protected val nodeSettings: NodeSettings = _nodeSettings
          override protected val storage: Store = db
        }
      case (false, true) =>
        new EncryHistory with FullStateProofProcessor with BlockPayloadProcessor {
          override protected val chainSettings: ChainSettings = settings.chainSettings
          override protected val nodeSettings: NodeSettings = _nodeSettings
          override protected val storage: Store = db
        }
      case (true, false) =>
        new EncryHistory with ADStateProofProcessor with EmptyBlockPayloadProcessor {
          override protected val chainSettings: ChainSettings = settings.chainSettings
          override protected val nodeSettings: NodeSettings = _nodeSettings
          override protected val storage: Store = db
        }
      case m =>
        throw new Error(s"Unsupported settings combination ADState==${m._1}, verifyTransactions==${m._2}, ")
    }
    history
  }
}
