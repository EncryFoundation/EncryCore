package encry.view.history

import encry.consensus.History.{Equal, Fork, HistoryComparisonResult, Older, Unknown, Younger}
import encry.modifiers.history.HeaderChain
import org.encryfoundation.common.modifiers.history.{Header, Payload}
import org.encryfoundation.common.utils.TaggedTypes.{Difficulty, Height, ModifierId, ModifierTypeId}
import org.encryfoundation.common.utils.constants.TestNetConstants
import scala.annotation.tailrec
import scala.collection.immutable.HashSet
import cats.syntax.option._
import encry.consensus.PowLinearController
import encry.settings.EncryAppSettings
import encry.view.history.HistoryValidationError.HistoryExtensionError
import org.encryfoundation.common.network.SyncInfo
import cats.syntax.either._
import encry.utils.NetworkTimeProvider
import org.encryfoundation.common.utils.Algos

trait HistoryExtension extends HistoryAPI {

  val settings: EncryAppSettings
  val timeProvider = new NetworkTimeProvider(settings.ntp)

  var blockDownloadProcessor: BlockDownloadProcessor = BlockDownloadProcessor.empty(settings.node)

  def payloadsToDownload(howMany: Int, excluding: HashSet[ModifierId]): (ModifierTypeId, Seq[ModifierId]) = {
    @tailrec def continuation(height: Int, acc: Seq[ModifierId]): Seq[ModifierId] =
      if (acc.lengthCompare(howMany) >= 0) acc
      else bestHeaderIdAtHeight(height).flatMap(getHeaderById) match {
        case Some(h) if !excluding.exists(_ sameElements h.payloadId) && !isModifierDefined(h.payloadId) =>
          logger.debug(s"Find new payload id ${Algos.encode(h.payloadId)} for best header at height $height")
          continuation(height + 1, acc :+ h.payloadId)
        case Some(h) =>
          logger.debug(s"Found payload id ${Algos.encode(h.payloadId)} for header at height $height " +
            s"with id ${h.encodedId} already contains in history or in excluding set")
          continuation(height + 1, acc)
        case None =>
          logger.debug(s"No best header at height $height. Returning all found payload ids")
          acc
      }

    (for {
      bestBlockId             <- getBestBlockIdOpt
      headerLinkedToBestBlock <- getHeaderById(bestBlockId)
    } yield headerLinkedToBestBlock) match {
      case _ if !blockDownloadProcessor.isHeadersChainSynced =>
        logger.debug(s"Header chain is not synced. Do not start asking payloads.")
        (Payload.modifierTypeId, Seq.empty)
      case Some(header) if isInBestChain(header) =>
        logger.debug(s"Found best header ${header.encodedId} at height ${header.height}. " +
          s"Start calculating payloads for download")
        (Payload.modifierTypeId, continuation(header.height + 1, Seq.empty))
      case Some(header) =>
        logger.debug(s"Found header is not from best chain. " +
          s"Starting process of finding last best height and calculating payloads for download")
        (Payload.modifierTypeId, lastBestBlockHeightRelevantToBestChain(header.height)
          .map(height => continuation(height + 1, Seq.empty))
          .getOrElse(continuation(blockDownloadProcessor.minimalBlockHeight, Seq.empty)))
      case None =>
        logger.debug(s"No best header linked to best block. Start asking payload from genesis height." +
          s" Current minimalBlockHeight is ${blockDownloadProcessor.minimalBlockHeight}")
        (Payload.modifierTypeId, continuation(blockDownloadProcessor.minimalBlockHeight, Seq.empty))
    }
  }

  /**
    * Checks, whether it's time to download full chain and return toDownload modifier
    */
  def toDownload(header: Header): Option[(ModifierTypeId, ModifierId)] =
    // Already synced and header is not too far back. Download required modifiers
    if (header.height >= blockDownloadProcessor.minimalBlockHeight) (Payload.modifierTypeId, header.payloadId).some
    // Headers chain is synced after this header. Start downloading full blocks
    else if (!blockDownloadProcessor.isHeadersChainSynced && isNewHeader(header)) {
      val updatedBlockDownloadProcessor: BlockDownloadProcessor =
        blockDownloadProcessor.chainIsSynced
      val (_, newBlockDownloadProcessor: BlockDownloadProcessor) =
        updatedBlockDownloadProcessor.updateBestBlock(header.height)
      blockDownloadProcessor = newBlockDownloadProcessor
      logger.info(s"Header chain is now synced. Best block height is ${blockDownloadProcessor.minimalBlockHeight}")
      none
    } else none

  /**
    * @param limit       - maximum length of resulting HeaderChain
    * @param startHeader - header to start
    * @param until       - stop condition
    * @return at most limit header back in history starting from startHeader and when condition until is not satisfied
    *         Note now it includes one header satisfying until condition!
    */
  def headerChainBack(limit: Int, startHeader: Header, until: Header => Boolean): HeaderChain = {
    @tailrec def loop(header: Header, acc: Seq[Header]): Seq[Header] =
      if (acc.length == limit || until(header)) acc
      else getHeaderById(header.parentId) match {
        case Some(parent)                 => loop(parent, acc :+ parent)
        case None if acc.contains(header) => acc
        case _                            => acc :+ header
      }

    if (getBestHeaderIdOpt.isEmpty || (limit == 0)) HeaderChain.empty
    else HeaderChain(loop(startHeader, Seq(startHeader)).reverse)
  }

  /**
    * Find first header with the best height <= $height which id satisfies predicate p
    *
    * @param height - start height
    * @param p      - condition to satisfy
    * @return found header
    */
  @tailrec final def loopHeightDown(height: Int, p: ModifierId => Boolean): Option[Header] = headerIdsAtHeight(height)
    .find(p)
    .flatMap(getHeaderById) match {
      case h@Some(_)                                       => h
      case None if height > TestNetConstants.GenesisHeight => loopHeightDown(height - 1, p)
      case n@None                                          => n
  }

  def requiredDifficultyAfter(parent: Header): Either[HistoryExtensionError, Difficulty] = {
    val requiredHeights: Seq[Height] = PowLinearController.getHeightsForReTargetingAt(Height @@ (parent.height + 1))
    for {
      _ <- Either.cond(requiredHeights.lastOption.contains(parent.height), (),
        HistoryExtensionError("Incorrect heights sequence!"))
      chain           = headerChainBack(requiredHeights.max - requiredHeights.min + 1, parent, (_: Header) => false)
      requiredHeaders = (requiredHeights.min to requiredHeights.max)
        .zip(chain.headers)
        .filter(p => requiredHeights.contains(p._1))
      _ <- Either.cond(requiredHeights.length == requiredHeaders.length, (),
        HistoryExtensionError(s"Missed headers: $requiredHeights != ${requiredHeaders.map(_._1)}"))
    } yield PowLinearController.getDifficulty(requiredHeaders)
  }


  /**
    * @return all possible forks, that contains specified header
    **/
  def continuationHeaderChains(header: Header, filterCond: Header => Boolean): Seq[Seq[Header]] = {
    @tailrec def loop(currentHeight: Int, acc: Seq[Seq[Header]]): Seq[Seq[Header]] = {
      val nextHeightHeaders: Seq[Header] = headerIdsAtHeight(currentHeight + 1)
        .flatMap(getHeaderById)
        .filter(filterCond)
      if (nextHeightHeaders.isEmpty) {
        logger.debug(s"Next height ${currentHeight + 1} headers are empty. Return acc of size ${acc.size}")
        acc.map(_.reverse)
      }
      else {
        val updatedChains: Seq[Seq[Header]] = nextHeightHeaders.flatMap(h =>
          acc.find(chain => chain.nonEmpty && h.parentId.sameElements(chain.head.id)).map(h +: _) //todo remove .head
        )
        val nonUpdatedChains: Seq[Seq[Header]] = acc.filter(chain =>
          !nextHeightHeaders.exists(_.parentId.sameElements(chain.head.id)) //todo remove .head
        )
        loop(currentHeight + 1, updatedChains ++ nonUpdatedChains)
      }
    }

    loop(header.height, Seq(Seq(header)))
  }

  def continuationIds(info: SyncInfo, size: Int): Either[HistoryExtensionError, Seq[(ModifierTypeId, ModifierId)]] =
    if (getBestHeaderIdOpt.isEmpty) info.startingPoints.asRight[HistoryExtensionError]
    else if (info.lastHeaderIds.isEmpty) {
      val heightFrom: Int = Math.min(getBestHeaderHeight, size - 1)
      val headerChainOpt: Option[HeaderChain] = for {
        startId     <- headerIdsAtHeight(heightFrom).headOption
        startHeader <- getHeaderById(startId)
      } yield headerChainBack(size, startHeader, _ => false)
      Either.cond(
        headerChainOpt.nonEmpty && headerChainOpt.exists(_.headers.exists(_.height == TestNetConstants.GenesisHeight)),
        headerChainOpt.getOrElse(HeaderChain.empty).headers.map(h => Header.modifierTypeId -> h.id),
        HistoryExtensionError("Should always contain genesis header")
      )
    } else {
      val ids: Seq[ModifierId] = info.lastHeaderIds
      (for {
        lastHeaderInOurBestChain <- ids.view.reverse.find(isInBestChain)
        theirHeight              <- heightOf(lastHeaderInOurBestChain)
        heightFrom = Math.min(getBestHeaderHeight, theirHeight + size)
        startId                  <- bestHeaderIdAtHeight(heightFrom)
        startHeader              <- getHeaderById(startId)
      } yield headerChainBack(size, startHeader, h => h.parentId sameElements lastHeaderInOurBestChain)
          .headers
          .map(h => Header.modifierTypeId -> h.id))
        .getOrElse(Seq.empty).asRight[HistoryExtensionError]
    }

  /**
    * Whether another's node syncInfo shows that another node is ahead or behind ours
    *
    * @param si other's node sync info
    * @return Equal if nodes have the same history, Younger if another node is behind, Older if a new node is ahead
    */
  def compare(si: SyncInfo): HistoryComparisonResult = getBestHeaderIdOpt match {
    //Our best header is the same as other history best header
    case Some(id) if si.lastHeaderIds.lastOption.exists(_ sameElements id) => Equal
    //Our best header is in other history best chain, but not at the last position
    case Some(id) if si.lastHeaderIds.exists(_ sameElements id) => Older
    //Other history is empty, or our history contains last id from other history
    case Some(_) if si.lastHeaderIds.isEmpty || si.lastHeaderIds.lastOption.exists(isModifierDefined) => Younger
    case Some(_) =>
      //Our history contains some ids from other history
      if (si.lastHeaderIds.exists(isModifierDefined)) Fork
      //Unknown comparison result
      else Unknown
    //Both nodes do not keep any blocks
    case None if si.lastHeaderIds.isEmpty => Equal
    //Our history is empty, other contain some headers
    case None => Older
  }

  def lastHeaders(count: Int): HeaderChain = getBestHeaderOpt
    .map(bestHeader => headerChainBack(count, bestHeader, _ => false))
    .getOrElse(HeaderChain.empty)

  /**
    * Return headers, required to apply to reach header2 if you are at header1 position.
    *
    * @param fromHeaderOpt - initial position
    * @param toHeader      - header you should reach
    * @return (Modifier required to rollback first, header chain to apply)
    */
  def getChainToHeader(fromHeaderOpt: Option[Header],
                       toHeader: Header): (Option[ModifierId], HeaderChain) = fromHeaderOpt match {
    case Some(h1) =>
      val chains: Either[HistoryExtensionError, (HeaderChain, HeaderChain)] = commonBlockThenSuffixes(h1, toHeader)
      chains
        .map { case (prevChain, newChain) => (prevChain.headOption.map(_.id), newChain.tail) }
        .getOrElse((none, HeaderChain.empty))
    case None => (none, headerChainBack(toHeader.height + 1, toHeader, _ => false))
  }

  /** Finds common block and sub-chains from common block to header1 and header2. */
  def commonBlockThenSuffixes(header1: Header,
                              header2: Header): Either[HistoryExtensionError, (HeaderChain, HeaderChain)] = {
    val heightDelta: Int = Math.max(header1.height - header2.height, 0)

    def loop(numberBack: Int, otherChain: HeaderChain): Either[HistoryExtensionError, (HeaderChain, HeaderChain)] = {
      val (firstChain: HeaderChain, secondChain: HeaderChain) =
        commonBlockThenSuffixes(otherChain, header1, numberBack + heightDelta)
      if (firstChain.headOption.exists(secondChain.headOption.contains))
        (firstChain, secondChain).asRight[HistoryExtensionError]
      else {
        val biggerOther: HeaderChain = headerChainBack(numberBack, otherChain.head, _ => false) ++ otherChain.tail
        if (!otherChain.headOption.exists(_.isGenesis)) loop(biggerOther.length, biggerOther)
        else HistoryExtensionError(s"Common point not found for headers $header1 and $header2")
          .asLeft[(HeaderChain, HeaderChain)]
      }
    }

    /** Finds common block and sub-chains with `otherChain`. */
    def commonBlockThenSuffixes(otherChain: HeaderChain,
                                startHeader: Header,
                                limit: Int): (HeaderChain, HeaderChain) = {
      def until: Header => Boolean = h => otherChain.exists(_.id sameElements h.id)

      val currentChain: HeaderChain = headerChainBack(limit, startHeader, until)
      (currentChain, otherChain.takeAfter(currentChain.head))
    }

    loop(numberBack = 2, HeaderChain(Seq(header2)))
  }

  def syncInfo: SyncInfo =
    if (getBestHeaderIdOpt.isEmpty) SyncInfo(Seq.empty)
    else SyncInfo(
      getBestHeaderOpt.map(header =>
        ((header.height - settings.network.maxInvObjects + 1) to header.height).flatMap(bestHeaderIdAtHeight)
      ).getOrElse(Seq.empty)
    )

  private def isNewHeader(header: Header): Boolean =
    timeProvider.estimatedTime - header.timestamp <
      TestNetConstants.DesiredBlockInterval.toMillis * TestNetConstants.NewHeaderTimeMultiplier
}