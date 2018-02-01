package encry.view.history

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.ADProofs
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.{EncryBlockHeader, EncryHeaderChain}
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.settings.{ChainSettings, NodeSettings}
import encry.view.history.processors.BlockHeaderProcessor
import encry.view.history.processors.payload.BaseBlockPayloadProcessor
import encry.view.history.processors.proofs.BaseADProofProcessor
import encry.view.history.storage.HistoryStorage
import io.iohk.iodb.Store
import scorex.core.consensus.History.{HistoryComparisonResult, ModifierIds}
import scorex.core.consensus.{History, HistoryReader, ModifierSemanticValidity}
import scorex.core.utils.ScorexLogging
import scorex.core.{ModifierId, ModifierTypeId}

import scala.util.{Failure, Try}

trait EncryHistoryReader
  extends HistoryReader[EncryPersistentModifier, EncrySyncInfo]
    with BlockHeaderProcessor
    with BaseBlockPayloadProcessor
    with BaseADProofProcessor
    with ScorexLogging {

  protected val chainSettings: ChainSettings
  protected val nodeSettings: NodeSettings

  protected val storage: Store

  override protected lazy val historyStorage: HistoryStorage = new HistoryStorage(storage)

  /**
    * Is there's no history, even genesis block
    */
  def isEmpty: Boolean = bestHeaderIdOpt.isEmpty

  /**
    * Header of best Header chain. Empty if no genesis block is applied yet (from a chain or a PoPoW proof).
    * Transactions and ADProofs for this Header may be missed, to get block from best full chain (in mode that support
    * it) call bestFullBlockOpt.
    */
  def bestHeaderOpt: Option[EncryBlockHeader] = bestHeaderIdOpt.flatMap(typedModifierById[EncryBlockHeader])

  /**
    * Complete block of the best chain with transactions.
    * Always None for an SPV mode, Some(fullBLock) for fullnode regime after initial bootstrap.
    */
  def bestFullBlockOpt: Option[EncryBlock] =
    bestFullBlockIdOpt.flatMap(id => typedModifierById[EncryBlockHeader](id)).flatMap(getFullBlock)

  /**
    * @return ids of count headers starting from offset
    */
  def getHeaderIds(count: Int, offset: Int = 0): Seq[ModifierId] = (offset until (count + offset))
    .flatMap(h => headerIdsAtHeight(h).headOption)

  /**
    * Id of best block to mine
    */
  override def openSurfaceIds(): Seq[ModifierId] = bestFullBlockIdOpt.orElse(bestHeaderIdOpt).toSeq

  // Compares node`s `SyncInfo` with another`s.
  override def compare(other: EncrySyncInfo): History.HistoryComparisonResult.Value = {
    bestHeaderIdOpt match {
      case Some(id) if other.lastHeaderIds.lastOption.exists(_ sameElements id) =>
        HistoryComparisonResult.Equal
      case Some(id) if other.lastHeaderIds.exists(_ sameElements id) =>
        HistoryComparisonResult.Older
      case Some(_) if other.lastHeaderIds.isEmpty =>
        HistoryComparisonResult.Younger
      case Some(_) =>
        // Compare headers chain
        val ids = other.lastHeaderIds
        ids.view.reverse.find(m => contains(m)) match {
          case Some(_) =>
            HistoryComparisonResult.Younger
          case None => HistoryComparisonResult.Nonsense
        }
      case None =>
        log.warn("Trying to compare with other node while our history is empty")
        HistoryComparisonResult.Older
    }
  }

  /**
    * @param info other's node sync info
    * @param size max return size
    * @return Ids of headerss, that node with info should download and apply to synchronize
    */
  override def continuationIds(info: EncrySyncInfo, size: Int): Option[ModifierIds] = Try {
    if (isEmpty) {
      info.startingPoints
    } else if (info.lastHeaderIds.isEmpty) {
      val heightFrom = Math.min(bestHeaderHeight, size - 1)
      val startId = headerIdsAtHeight(heightFrom).head
      val startHeader = typedModifierById[EncryBlockHeader](startId).get
      val headers = headerChainBack(size, startHeader, _ => false)
        .ensuring(_.headers.exists(_.height == 0), "Should always contain genesis header")
      headers.headers.flatMap(h => Seq((EncryBlockHeader.modifierTypeId, h.id)))
    } else {
      val ids = info.lastHeaderIds
      val lastHeaderInOurBestChain: ModifierId = ids.view.reverse.find(m => isInBestChain(m)).get
      val theirHeight = heightOf(lastHeaderInOurBestChain).get
      val heightFrom = Math.min(bestHeaderHeight, theirHeight + size)
      val startId = headerIdsAtHeight(heightFrom).head
      val startHeader = typedModifierById[EncryBlockHeader](startId).get
      val headerIds = headerChainBack(size, startHeader, h => h.parentId sameElements lastHeaderInOurBestChain)
        .headers.map(h => EncryBlockHeader.modifierTypeId -> h.id)
      headerIds
    }
  }.toOption

  /**
    * @return all possible forks, that contains specified header
    */
  protected[history] def continuationHeaderChains(header: EncryBlockHeader): Seq[EncryHeaderChain] = {
    def loop(acc: Seq[EncryBlockHeader]): Seq[EncryHeaderChain] = {
      val bestHeader = acc.last
      val currentHeight = heightOf(bestHeader.id).get
      val nextLevelHeaders = headerIdsAtHeight(currentHeight + 1).map(id => typedModifierById[EncryBlockHeader](id).get)
        .filter(_.parentId sameElements bestHeader.id)
      if (nextLevelHeaders.isEmpty) Seq(EncryHeaderChain(acc))
      else nextLevelHeaders.map(h => acc :+ h).flatMap(chain => loop(chain))
    }

    loop(Seq(header))
  }

  protected def testApplicable(modifier: EncryPersistentModifier): Try[Unit] = {
    modifier match {
      case header: EncryBlockHeader => validate(header)
      case payload: EncryBlockPayload => validate(payload)
      case adProofs: ADProofs => validate(adProofs)
      case mod: Any => Failure(new Error(s"Modifier $mod is of incorrect type."))
    }
  }

  // Checks whether the `modifier` is applicable to the `history`.
  override def applicable(modifier: EncryPersistentModifier): Boolean = testApplicable(modifier).isSuccess

  def lastHeaders(count: Int): EncryHeaderChain = bestHeaderOpt
    .map(bestHeader => headerChainBack(count, bestHeader, _ => false)).getOrElse(EncryHeaderChain.empty)

  // Gets EncryPersistentModifier by it's id if it is in history.
  override def modifierById(id: ModifierId): Option[EncryPersistentModifier] =
    historyStorage.modifierById(id) match {
      case Some(mod) => if (mod.id sameElements id) Some(mod) else None
      case _ => None
    }

  // Gets EncryPersistentModifier of type T by it's id if it is in history.
  def typedModifierById[T <: EncryPersistentModifier](id: ModifierId): Option[T] = modifierById(id) match {
    case Some(m: T@unchecked) if m.isInstanceOf[T] => Some(m)
    case _ => None
  }

  def getFullBlock(header: EncryBlockHeader): Option[EncryBlock] = {
    val aDProofs = typedModifierById[ADProofs](header.adProofsId)
    typedModifierById[EncryBlockPayload](header.payloadId).map { txs =>
      new EncryBlock(header, txs, aDProofs)
    }
  }

  def missedModifiersForFullChain(): Seq[(ModifierTypeId, ModifierId)] = {
    if (nodeSettings.verifyTransactions) {
      bestHeaderOpt.toSeq
        .flatMap(h => headerChainBack(bestHeaderHeight + 1, h, p => contains(p.adProofsId) && contains(p.payloadId)).headers)
        .flatMap(h => Seq((EncryBlockPayload.modifierTypeId, h.payloadId), (ADProofs.modifierTypeId, h.adProofsId)))
        .filter(id => !contains(id._2))
    }
    else {
      Seq()
    }
  }

  def fullBlocksAfter(fromBlockOpt: Option[EncryBlock]): Try[Seq[EncryBlock]] = Try {
    bestFullBlockOpt match {
      case Some(bestFull) if !fromBlockOpt.contains(bestFull) =>
        val until = (h: EncryBlockHeader) => fromBlockOpt.exists(fb => h.parentId sameElements fb.header.id)
        headerChainBack(bestFull.header.height + 1, bestFull.header, until).headers
          .map(h => getFullBlock(h).get)
      case _ =>
        Seq()
    }
  }

  protected[history] def commonBlockThenSuffixes(header1: EncryBlockHeader,
                                                 header2: EncryBlockHeader): (EncryHeaderChain, EncryHeaderChain) = {
    assert(contains(header1))
    assert(contains(header2))

    def loop(numberBack: Int, otherChain: EncryHeaderChain): (EncryHeaderChain, EncryHeaderChain) = {
      val r = commonBlockThenSuffixes(otherChain, header1, numberBack)
      if (r._1.head == r._2.head) {
        r
      } else {
        val biggerOther = headerChainBack(numberBack, otherChain.head, _ => false) ++ otherChain.tail
        if (!otherChain.head.isGenesis) {
          loop(biggerOther.size, biggerOther)
        } else {
          throw new Error(s"Common point not found for headers $header1 and $header2")
        }
      }
    }

    loop(2, EncryHeaderChain(Seq(header2)))
  }

  protected[history] def commonBlockThenSuffixes(otherChain: EncryHeaderChain,
                                                 startHeader: EncryBlockHeader,
                                                 limit: Int): (EncryHeaderChain, EncryHeaderChain) = {
    def until(h: EncryBlockHeader): Boolean = otherChain.exists(_.id sameElements h.id)

    val ourChain = headerChainBack(limit, startHeader, until)
    val commonBlock = ourChain.head
    val commonBlockThenSuffixes = otherChain.takeAfter(commonBlock)
    (ourChain, commonBlockThenSuffixes)
  }

  override def syncInfo: EncrySyncInfo = if (isEmpty) {
    EncrySyncInfo(Seq())
  } else {
    EncrySyncInfo(lastHeaders(EncrySyncInfo.MaxBlockIds).headers.map(_.id))
  }

  override def isSemanticallyValid(modifierId: ModifierId): ModifierSemanticValidity.Value = {
    historyStorage.db.get(validityKey(modifierId)) match {
      case Some(b) if b.data.headOption.contains(1.toByte) => ModifierSemanticValidity.Valid
      case Some(b) if b.data.headOption.contains(0.toByte) => ModifierSemanticValidity.Invalid
      case None if contains(modifierId) => ModifierSemanticValidity.Unknown
      case None => ModifierSemanticValidity.Absent
      case m =>
        log.error(s"Incorrect validity status: $m")
        ModifierSemanticValidity.Absent
    }
  }
}
