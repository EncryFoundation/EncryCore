package encry.view.history

import encry._
import encry.consensus.History._
import encry.consensus.ModifierSemanticValidity
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.ADProofs
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.{EncryBlockHeader, EncryHeaderChain}
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.settings.{Algos, Constants, NodeSettings}
import encry.utils.Logging
import encry.view.history.processors.BlockHeaderProcessor
import encry.view.history.processors.payload.BaseBlockPayloadProcessor
import encry.view.history.processors.proofs.BaseADProofProcessor
import encry.EncryApp.settings
import scala.annotation.tailrec
import scala.util.{Failure, Try}

trait EncryHistoryReader extends BlockHeaderProcessor with BaseBlockPayloadProcessor with BaseADProofProcessor with Logging {

  protected val nodeSettings: NodeSettings

  /** Is there's no history, even genesis block */
  def isEmpty: Boolean = bestHeaderIdOpt.isEmpty

  def contains(id: ModifierId): Boolean = modifierById(id).isDefined

  /**
    * Header of best Header chain. Empty if no genesis block is applied yet.
    * Transactions and ADProofs for this Header may be missed, to get block from best full chain (in mode that support
    * it) call bestFullBlockOpt.
    */
  def bestHeaderOpt: Option[EncryBlockHeader] = bestHeaderIdOpt.flatMap(typedModifierById[EncryBlockHeader])

  /**
    * Complete block of the best chain with transactions.
    * Always None for an SPV mode, Some(fullBLock) for fullnode regime after initial bootstrap.
    */
  def bestBlockOpt: Option[EncryBlock] =
    bestBlockIdOpt.flatMap(id => typedModifierById[EncryBlockHeader](id)).flatMap(getBlock)

  /** @return ids of count headers starting from offset */
  def getHeaderIds(count: Int, offset: Int = 0): Seq[ModifierId] = (offset until (count + offset))
    .flatMap(h => headerIdsAtHeight(h).headOption)

  /**
    * Whether another's node syncinfo shows that another node is ahead or behind ours
    *
    * @param si other's node sync info
    * @return Equal if nodes have the same history, Younger if another node is behind, Older if a new node is ahead
    */
  def compare(si: EncrySyncInfo): HistoryComparisonResult = {
    bestHeaderIdOpt match {
      case Some(id) if si.lastHeaderIds.lastOption.exists(_ sameElements id) =>
        Equal //Our best header is the same as other node best header
      case Some(id) if si.lastHeaderIds.exists(_ sameElements id) =>
        Older //Our best header is in other node best chain, but not at the last position
      case Some(_) if si.lastHeaderIds.isEmpty =>
        Younger //Other history is empty, our contain some headers
      case Some(_) =>
        //We are on different forks now.
        if (si.lastHeaderIds.view.reverse.exists(m => contains(m)))
          Younger //Return Younger, because we can send blocks from our fork that other node can download.
        else Unknown //We don't have any of id's from other's node sync info in history.
      //We don't know whether we can sync with it and what blocks to send in Inv message.
      case None if si.lastHeaderIds.isEmpty => Equal //Both nodes do not keep any blocks
      case None => Older //Our history is empty, other contain some headers
    }
  }

  def continuationIds(info: EncrySyncInfo, size: Int): Option[ModifierIds] = Try {
    if (isEmpty) info.startingPoints
    else if (info.lastHeaderIds.isEmpty) {
      val heightFrom: Int = Math.min(bestHeaderHeight, size - 1)
      val startId: ModifierId = headerIdsAtHeight(heightFrom).head
      val startHeader: EncryBlockHeader = typedModifierById[EncryBlockHeader](startId).get
      val headers: EncryHeaderChain = headerChainBack(size, startHeader, _ => false)
        .ensuring(_.headers.exists(_.height == Constants.Chain.GenesisHeight), "Should always contain genesis header")
      headers.headers.flatMap(h => Seq((EncryBlockHeader.modifierTypeId, h.id)))
    } else {
      val ids: Seq[ModifierId] = info.lastHeaderIds
      val lastHeaderInOurBestChain: ModifierId = ids.view.reverse.find(m => isInBestChain(m)).get
      val theirHeight: Height = heightOf(lastHeaderInOurBestChain).get
      val heightFrom: Int = Math.min(bestHeaderHeight, theirHeight + size)
      val startId: ModifierId = headerIdsAtHeight(heightFrom).head
      val startHeader: EncryBlockHeader = typedModifierById[EncryBlockHeader](startId).get
      headerChainBack(size, startHeader, h => h.parentId sameElements lastHeaderInOurBestChain)
        .headers.map(h => EncryBlockHeader.modifierTypeId -> h.id)
    }
  }.toOption

  /** @return all possible forks, that contains specified header */
  protected[history] def continuationHeaderChains(header: EncryBlockHeader,
                                                  filterCond: EncryBlockHeader => Boolean): Seq[Seq[EncryBlockHeader]] = {
    @tailrec
    def loop(currentHeight: Option[Int], acc: Seq[Seq[EncryBlockHeader]]): Seq[Seq[EncryBlockHeader]] = {
      val nextLevelHeaders: Seq[EncryBlockHeader] = currentHeight.toList
        .flatMap { h => headerIdsAtHeight(h + 1) }
        .flatMap { id => typedModifierById[EncryBlockHeader](id) }
        .filter(filterCond)
      if (nextLevelHeaders.isEmpty) acc.map(chain => chain.reverse)
      else {
        val updatedChains: Seq[Seq[EncryBlockHeader]] = nextLevelHeaders.flatMap { h =>
          acc.find(chain => chain.nonEmpty && (h.parentId sameElements chain.head.id)).map(c => h +: c)
        }
        val nonUpdatedChains: Seq[Seq[EncryBlockHeader]] = acc.filter(chain => !nextLevelHeaders.exists(_.parentId sameElements chain.head.id))
        loop(currentHeight.map(_ + 1), updatedChains ++ nonUpdatedChains)
      }
    }

    //TODO: Remove Some
    loop(Some(header.height), Seq(Seq(header)))
  }

  def testApplicable(modifier: EncryPersistentModifier): Try[Unit] = modifier match {
    case header: EncryBlockHeader => validate(header)
    case payload: EncryBlockPayload => validate(payload)
    case adProofs: ADProofs => validate(adProofs)
    case mod: Any => Failure(new Exception(s"Modifier $mod is of incorrect type."))
  }

  /** Checks whether the modifier is applicable to the history. */
  def applicable(modifier: EncryPersistentModifier): Boolean = testApplicable(modifier).isSuccess

  def lastHeaders(count: Int): EncryHeaderChain = bestHeaderOpt
    .map(bestHeader => headerChainBack(count, bestHeader, _ => false)).getOrElse(EncryHeaderChain.empty)

  def modifierById(id: ModifierId): Option[EncryPersistentModifier] =
    historyStorage.modifierById(id)
      .ensuring(_.forall(_.id sameElements id), s"Modifier ${Algos.encode(id)} id mismatch")

  def typedModifierById[T <: EncryPersistentModifier](id: ModifierId): Option[T] = modifierById(id) match {
    case Some(m: T@unchecked) if m.isInstanceOf[T] => Some(m)
    case _ => None
  }

  def getBlock(header: EncryBlockHeader): Option[EncryBlock] =
    (typedModifierById[EncryBlockPayload](header.payloadId), typedModifierById[ADProofs](header.adProofsId)) match {
      case (Some(txs), Some(proofs)) => Some(EncryBlock(header, txs, Some(proofs)))
      case (Some(txs), None) if !nodeSettings.stateMode.isDigest => Some(EncryBlock(header, txs, None))
      case _ => None
    }

  def missedModifiersForFullChain: Seq[(ModifierTypeId, ModifierId)] = if (nodeSettings.verifyTransactions) {
    bestHeaderOpt.toSeq
      .flatMap(h => headerChainBack(bestHeaderHeight + 1, h, _ => false).headers)
      .flatMap(h => Seq((EncryBlockPayload.modifierTypeId, h.payloadId), (ADProofs.modifierTypeId, h.adProofsId)))
      .filter(id => !contains(id._2))
  } else Seq.empty

  /**
    * Return headers, required to apply to reach header2 if you are at header1 position.
    *
    * @param fromHeaderOpt - initial position
    * @param toHeader      - header you should reach
    * @return (Modifier required to rollback first, header chain to apply)
    */
  def getChainToHeader(fromHeaderOpt: Option[EncryBlockHeader],
                       toHeader: EncryBlockHeader): (Option[ModifierId], EncryHeaderChain) = fromHeaderOpt match {
    case Some(h1) =>
      val (prevChain, newChain) = commonBlockThenSuffixes(h1, toHeader)
      (prevChain.headOption.map(_.id), newChain.tail)
    case None => (None, headerChainBack(toHeader.height + 1, toHeader, _ => false))
  }

  /** Finds common block and sub-chains from common block to header1 and header2. */
  protected[history] def commonBlockThenSuffixes(header1: EncryBlockHeader,
                                                 header2: EncryBlockHeader): (EncryHeaderChain, EncryHeaderChain) = {
    val heightDelta: Int = Math.max(header1.height - header2.height, 0)

    def loop(numberBack: Int, otherChain: EncryHeaderChain): (EncryHeaderChain, EncryHeaderChain) = {
      val chains: (EncryHeaderChain, EncryHeaderChain) = commonBlockThenSuffixes(otherChain, header1, numberBack + heightDelta)
      if (chains._1.head == chains._2.head) chains
      else {
        val biggerOther: EncryHeaderChain = headerChainBack(numberBack, otherChain.head, _ => false) ++ otherChain.tail
        if (!otherChain.head.isGenesis) loop(biggerOther.size, biggerOther)
        else throw new Exception(s"Common point not found for headers $header1 and $header2")
      }
    }

    loop(2, EncryHeaderChain(Seq(header2)))
  }

  /** Finds common block and sub-chains with `otherChain`. */
  protected[history] def commonBlockThenSuffixes(otherChain: EncryHeaderChain,
                                                 startHeader: EncryBlockHeader,
                                                 limit: Int): (EncryHeaderChain, EncryHeaderChain) = {
    def until(h: EncryBlockHeader): Boolean = otherChain.exists(_.id sameElements h.id)

    val currentChain: EncryHeaderChain = headerChainBack(limit, startHeader, until)
    (currentChain, otherChain.takeAfter(currentChain.head))
  }

  def syncInfo: EncrySyncInfo = if (isEmpty) EncrySyncInfo(Seq.empty)
  else EncrySyncInfo(lastHeaders(settings.network.syncPackageLength).headers.map(_.id))

  override def isSemanticallyValid(modifierId: ModifierId): ModifierSemanticValidity =
    historyStorage.store.get(validityKey(modifierId)) match {
      case Some(b) if b.data.headOption.contains(1.toByte) => ModifierSemanticValidity.Valid
      case Some(b) if b.data.headOption.contains(0.toByte) => ModifierSemanticValidity.Invalid
      case None if contains(modifierId) => ModifierSemanticValidity.Unknown
      case None => ModifierSemanticValidity.Absent
      case m =>
        logError(s"Incorrect validity status: $m")
        ModifierSemanticValidity.Absent
    }
}