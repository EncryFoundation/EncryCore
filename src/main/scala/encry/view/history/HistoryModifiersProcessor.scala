package encry.view.history

import cats.syntax.option._
import cats.syntax.either._
import encry.EncryApp.forceStopApplication
import encry.consensus.History.ProgressInfo
import encry.modifiers.history.HeaderChain
import encry.settings.EncryAppSettings
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{Block, Header, Payload}
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
import scala.annotation.tailrec
import scala.util.Try


trait HistoryModifiersProcessor extends HistoryExtension {

  val settings: EncryAppSettings

  def process(modifier: PersistentModifier): ProgressInfo = modifier match {
    case header: Header => processHeader(header)
    case payload: Payload => processPayload(payload)
  }

  private def processHeader(h: Header): ProgressInfo = getHeaderInfoUpdate(h) match {
    case dataToUpdate: Seq[_] if dataToUpdate.nonEmpty =>
      history.bulkInsert(h.id, dataToUpdate, Seq(h)) //side effect
      getBestHeaderIdOpt match {
        case Some(bestHeaderId) => //todo do we need seq(h) toDownload
          ProgressInfo(none, Seq.empty, if (!bestHeaderId.sameElements(h.id)) Seq.empty else Seq(h), toDownload(h))
        case _ => forceStopApplication(errorMessage = "Should always have best header after header application")
      }
    case _ => ProgressInfo(none, Seq.empty, Seq.empty, none)
  }

  private def processPayload(payload: Payload): ProgressInfo = getHeaderById(payload.headerId)
    .flatMap(h =>
      if (h.height - getBestBlockHeight >= 2 + settings.network.maxInvObjects) none
      else processBlock(Block(h, payload)).some
    )
    .getOrElse(putToHistory(payload))

  private def processBlock(block: Block): ProgressInfo = {
    val bestFullChain: Seq[Block] = calculateBestFullChain(block)
    //addBlockToCacheIfNecessary
    bestFullChain.lastOption.map(_.header) match {
      case Some(header) if isValidFirstBlock(block.header) =>
        processValidFirstBlock(block, block.payload, header, bestFullChain, settings.node.blocksToKeep)
      case Some(header) if getBestBlockIdOpt.nonEmpty && isBetterChain(header.id) =>
        processBetterChain(block, block.payload, header, Seq.empty, settings.node.blocksToKeep)
      case Some(header) =>
        processNonBestBlock(block, header, Seq.empty, settings.node.blocksToKeep)
      case None =>
        ProgressInfo(none, Seq.empty, Seq.empty, none) //todo new case
    }
  }

  private def processValidFirstBlock(fullBlock: Block,
                                     newModRow: PersistentModifier,
                                     newBestHeader: Header,
                                     newBestChain: Seq[Block],
                                     blocksToKeep: Int): ProgressInfo = {
    logger.info(s"Appending ${fullBlock.encodedId} as a valid first block")
    logStatus(Seq.empty, newBestChain, fullBlock, none)
    logStatus(0, Seq.empty, newBestChain.length, fullBlock, none)
    //toRemove: Seq[Block],
    //                        toApply: Seq[Block],
    //                        appliedBlock: Block,
    //                        prevBest: Option[Header]
    updateStorage(newModRow, newBestHeader.id)
    ProgressInfo(none, Seq.empty, newBestChain, none)
  }

  //todo toRemove will be fixed later
  private def processBetterChain(fullBlock: Block,
                                 newModRow: PersistentModifier,
                                 newBestHeader: Header,
                                 newBestChain: Seq[Block],
                                 blocksToKeep: Int): ProgressInfo = getHeaderOfBestBlock.map { header =>
    val (prevChain: HeaderChain, newChain: HeaderChain) = commonBlockThenSuffixes(header, newBestHeader)
    val toRemove: Seq[Block] = prevChain
      .tail
      .headers
      .flatMap(h => getBlockById(h.id))
    val toApply: Seq[Block] = newChain
      .tail
      .headers
      .flatMap(h => if (h == fullBlock.header) fullBlock.some else getBlockById(h.id))
    //toApply.foreach(addBlockToCacheIfNecessary)
    if (toApply.lengthCompare(newChain.length - 1) != 0)
      processNonBestBlock(fullBlock, header, Seq.empty, settings.node.blocksToKeep)
    else {
      //application of this block leads to full chain with higher score
      logger.info(s"Appending ${fullBlock.encodedId}|${fullBlock.header.height} as a better chain")
      logStatus(toRemove, toApply, fullBlock, Some(header))
      val branchPoint: Option[ModifierId] = toRemove.headOption.map(_ => prevChain.head.id) //todo remove .head
      val bestHeaderHeight: Int = getBestHeaderHeight
      val updateBestHeader: Boolean =
        (fullBlock.header.height > bestHeaderHeight) || (
          (fullBlock.header.height == bestHeaderHeight) &&
            scoreOf(fullBlock.id)
              .flatMap(fbScore => getBestHeaderIdOpt.flatMap(id => scoreOf(id).map(_ < fbScore)))
              .getOrElse(false))

      updateStorage(newModRow, newBestHeader.id, updateBestHeader)
      if (blocksToKeep >= 0) { //todo make calls safety
        val lastKept: Int = blockDownloadProcessor.updateBestBlock(fullBlock.header)
        val bestHeight: Int = toApply.last.header.height
        val diff: Int = bestHeight - header.height
        val clipResult: Either[Throwable, Unit] = clipBlockDataAt(((lastKept - diff) until lastKept).filter(_ >= 0))
      }
      ProgressInfo(branchPoint, toRemove, toApply, none)
    }
  }.getOrElse(ProgressInfo(none, Seq.empty, Seq.empty, none))

  private def processNonBestBlock(fullBlock: Block,
                                  newBestHeader: Header,
                                  newBestChain: Seq[Block],
                                  blocksToKeep: Int): ProgressInfo = {
    //Orphaned block or full chain is not initialized yet
    logStatus(Seq.empty, Seq.empty, fullBlock, none)
    history.bulkInsert(fullBlock.payload.id, Seq.empty, Seq(fullBlock.payload))
    ProgressInfo(none, Seq.empty, Seq.empty, none)
  }

  private def putToHistory(payload: Payload): ProgressInfo = {
    history.insertObjects(Seq(payload)) //side effect
    ProgressInfo(none, Seq.empty, Seq.empty, none)
  }

  private def clipBlockDataAt(heights: Seq[Int]): Either[Throwable, Unit] = Either.catchNonFatal {
    val toRemove: Seq[ModifierId] = heights
      .flatMap(h => headerIdsAtHeight(h))
      .flatMap(getHeaderById)
      .map(h => h.payloadId)
    history.removeObjects(toRemove)
  }

  private def calculateBestFullChain(block: Block): Seq[Block] = {
    val continuations: Seq[Seq[Header]] = continuationHeaderChains(block.header, h => isModifierDefined(h.id)).map(_.tail)
    logger.debug(s"continuations: ${continuations.map(seq => s"Seq contains: ${seq.length}").mkString(",")}")
    val chains: Seq[Seq[Block]] = continuations.map(_.filter(h => isModifierDefined(h.id)).flatMap(h => getBlockById(h.id)))
    logger.debug(s"Chains: ${chains.map(chain => s"chain contain: ${chain.length}").mkString(",")}")
    chains.map(c => block +: c).maxBy(c => scoreOf(c.last.id).get)
  }

  def isValidFirstBlock(header: Header): Boolean =
    header.height == blockDownloadProcessor.minimalBlockHeight && getBestBlockIdOpt.isEmpty

  private def isBetterChain(id: ModifierId): Boolean = (for {
    bestFullBlockId <- getBestBlockIdOpt
    heightOfThisHeader <- getModifierHeightById(id) //todo possible combine with getHeader(id: ModifierId)
    prevBestScore <- scoreOf(bestFullBlockId)
    score <- scoreOf(id)
    bestBlockHeight = getBestBlockHeight
  } yield (bestBlockHeight < heightOfThisHeader) || (bestBlockHeight == heightOfThisHeader && score > prevBestScore))
    .getOrElse(false)

  /** Finds common block and sub-chains from common block to header1 and header2. */
  private def commonBlockThenSuffixes(firstHeader: Header,
                                      secondHeader: Header): (HeaderChain, HeaderChain) = {
    val heightDelta: Int = Math.max(firstHeader.height - secondHeader.height, 0)

    def loop(numberBack: Int, otherChain: HeaderChain): (HeaderChain, HeaderChain) = {
      val chains: (HeaderChain, HeaderChain) = commonBlockThenSuffixes(otherChain, firstHeader, numberBack + heightDelta)
      if (chains._1.head == chains._2.head) chains
      else {
        val biggerOther: HeaderChain = headerChainBack(numberBack, otherChain.head, _ => false) ++ otherChain.tail
        if (!otherChain.head.isGenesis) loop(biggerOther.length, biggerOther)
        else throw new Exception(s"Common point not found for headers $firstHeader and $secondHeader")
      }
    }

    loop(2, HeaderChain(Seq(secondHeader)))
  }

  /** Finds common block and sub-chains with `otherChain`. */
  private def commonBlockThenSuffixes(otherChain: HeaderChain,
                                      startHeader: Header,
                                      limit: Int): (HeaderChain, HeaderChain) = {
    def until(h: Header): Boolean = otherChain.exists(_.id sameElements h.id)

    val currentChain: HeaderChain = headerChainBack(limit, startHeader, until)
    (currentChain, otherChain.takeAfter(currentChain.head))
  }

  /** @return all possible forks, that contains specified header */
  protected[history] def continuationHeaderChains(header: Header, filterCond: Header => Boolean): Seq[Seq[Header]] = {
    @tailrec
    def loop(currentHeight: Int, acc: Seq[Seq[Header]]): Seq[Seq[Header]] = {
      val nextLevelHeaders: Seq[Header] = Seq(currentHeight)
        .flatMap(h => headerIdsAtHeight(h + 1))
        .flatMap(getHeaderById)
        .filter(filterCond)
      if (nextLevelHeaders.isEmpty) acc.map(_.reverse)
      else {
        val updatedChains: Seq[Seq[Header]] = nextLevelHeaders.flatMap { h =>
          acc.find(chain => chain.nonEmpty && (h.parentId sameElements chain.head.id)).map(h +: _)
        }
        val nonUpdatedChains: Seq[Seq[Header]] = acc.filter(chain =>
          !nextLevelHeaders.exists(_.parentId sameElements chain.head.id))
        loop(currentHeight + 1, updatedChains ++ nonUpdatedChains)
      }
    }

    loop(header.height, Seq(Seq(header)))
  }

  private def updateStorage(newModRow: PersistentModifier,
                            bestFullHeaderId: ModifierId,
                            updateHeaderInfo: Boolean = false): Unit = {
    val indicesToInsert: Seq[(Array[Byte], Array[Byte])] =
      if (updateHeaderInfo) Seq(BestBlockKey -> bestFullHeaderId, BestHeaderKey -> bestFullHeaderId)
      else Seq(BestBlockKey -> bestFullHeaderId)
    history.bulkInsert(newModRow.id, indicesToInsert, Seq(newModRow))
  }

  private def logStatus(toRemoveLength: Int,
                        toApplyLastHeader: Seq[Header],
                        toApplyLength: Int,
                        appliedBlockEncodedId: String,
                        appliedBlockTxsLength: Int,
                        prevBest: Option[Header]): Unit = {
    val toRemoveStr: String = if (toRemoveLength == 0) "" else s" and to remove $toRemoveLength"
    val newStatusStr: String = if (toApplyLastHeader.isEmpty) "" else {
      s" New best block is ${toApplyLastHeader.last.encodedId} " +
        s"with height ${toApplyLastHeader.last.height} " +
        s"updates block ${prevBest.map(_.encodedId).getOrElse("None")} " +
        s"with height ${prevBest.map(_.height).getOrElse(-1)}"
    }
    logger.info(s"Full block $appliedBlockEncodedId appended (txs: $appliedBlockTxsLength), " +
      s"going to apply $toApplyLength, $toRemoveStr modifiers.$newStatusStr")
  }

}