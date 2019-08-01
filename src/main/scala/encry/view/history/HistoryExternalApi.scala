package encry.view.history

import cats.syntax.either._
import encry.consensus.History._
import encry.consensus._
import encry.modifiers.history._
import encry.settings.EncryAppSettings
import encry.utils.NetworkTimeProvider
import encry.view.history.processors.ValidationError.HistoryExternalApiError
import encry.view.history.processors.{BlockDownloadProcessor, ValidationError}
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.modifiers.history.{Block, Header, Payload}
import org.encryfoundation.common.network.SyncInfo
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{Difficulty, Height, ModifierId, ModifierTypeId}
import org.encryfoundation.common.utils.constants.TestNetConstants
import scala.annotation.tailrec
import scala.collection.immutable.HashSet
import scala.util.Try

trait HistoryExternalApi extends HistoryInternalApi {

  val settings: EncryAppSettings

  val timeProvider: NetworkTimeProvider

  var headersCacheIndexes: Map[Int, Seq[ModifierId]] = Map.empty[Int, Seq[ModifierId]]

  var headersCache: Map[ByteArrayWrapper, Header] = Map.empty[ByteArrayWrapper, Header]

  var blocksCacheIndexes: Map[Int, Seq[ModifierId]] = Map.empty[Int, Seq[ModifierId]]

  var blocksCache: Map[ByteArrayWrapper, Block] = Map.empty[ByteArrayWrapper, Block]

  lazy val blockDownloadProcessor: BlockDownloadProcessor = BlockDownloadProcessor(settings.node)

  private var isHeadersChainSyncedVar: Boolean = false

  def getHeaderById(id: ModifierId): Option[Header] = headersCache
    .get(ByteArrayWrapper(id))
    .orElse(blocksCache.get(ByteArrayWrapper(id)).map(_.header))
    .orElse(getHeaderByIdInternal(id))

  def getBlockByHeader(header: Header): Option[Block] = blocksCache
    .get(ByteArrayWrapper(header.id))
    .orElse(getPayloadByIdInternal(header.payloadId).map(p => Block(header, p)))

  def getBlockByHeaderId(id: ModifierId): Option[Block] = blocksCache
    .get(ByteArrayWrapper(id))
    .orElse(getHeaderById(id).flatMap(h => getPayloadByIdInternal(h.payloadId).map(p => Block(h, p))))

  def getBestHeader: Option[Header] = getBestHeaderId.flatMap(id =>
    headersCache
      .get(ByteArrayWrapper(id))
      .orElse(blocksCache.get(ByteArrayWrapper(id)).map(_.header))
      .orElse(getHeaderByIdInternal(id))
  )

  def getBestHeaderHeight: Int = getBestHeaderId.flatMap(id =>
    headersCache.get(ByteArrayWrapper(id)).map(_.height)
      .orElse(blocksCache.get(ByteArrayWrapper(id)).map(_.header.height))
      .orElse(getHeightByHeaderId(id))
  ).getOrElse(TestNetConstants.PreGenesisHeight)

  def getBestBlock: Option[Block] = getBestBlockId.flatMap(id =>
    blocksCache.get(ByteArrayWrapper(id))
      .orElse(getBlockByHeaderIdInternal(id))
  )

  def getBestBlockHeight: Int = getBestBlockId
    .flatMap(id => blocksCache.get(ByteArrayWrapper(id)).map(_.header.height).orElse(getHeightByHeaderId(id)))
    .getOrElse(TestNetConstants.PreGenesisHeight)

  def getHeaderOfBestBlock: Option[Header] = getBestBlockId.flatMap(id =>
    headersCache.get(ByteArrayWrapper(id))
      .orElse(blocksCache.get(ByteArrayWrapper(id)).map(_.header))
      .orElse(getHeaderByIdInternal(id))
  )

  //todo make this logic correct
  def isBlockDefined(header: Header): Boolean =
    blocksCache.get(ByteArrayWrapper(header.id)).isDefined || isModifierDefined(header.payloadId)

  //todo make this logic correct
  def isHeaderDefined(id: ModifierId): Boolean =
    headersCache.get(ByteArrayWrapper(id)).isDefined ||
      blocksCache.get(ByteArrayWrapper(id)).isDefined ||
      isModifierDefined(id)

  def payloadsIdsToDownload(howMany: Int, excluding: HashSet[ModifierId]): Seq[ModifierId] = {
    @tailrec def continuation(height: Int, acc: Seq[ModifierId]): Seq[ModifierId] =
      if (acc.lengthCompare(howMany) >= 0) acc
      else getBestHeaderIdAtHeight(height).flatMap(getHeaderById) match {
        case Some(h) if !excluding.exists(_.sameElements(h.payloadId)) && !isBlockDefined(h) => //todo changed from isModDefined
          continuation(height + 1, acc :+ h.payloadId)
        case Some(_) =>
          continuation(height + 1, acc)
        case None =>
          acc
      }

    (for {
      bestBlockId <- getBestBlockId
      headerLinkedToBestBlock <- getHeaderById(bestBlockId)
    } yield headerLinkedToBestBlock) match {
      case _ if !isHeadersChainSynced =>
        Seq.empty
      case Some(header) if isInBestChain(header) =>
        continuation(header.height + 1, Seq.empty)
      case Some(header) =>
        lastBestBlockHeightRelevantToBestChain(header.height)
          .map(height => continuation(height + 1, Seq.empty))
          .getOrElse(continuation(blockDownloadProcessor.minimalBlockHeightVar, Seq.empty))
      case None =>
        continuation(blockDownloadProcessor.minimalBlockHeightVar, Seq.empty)
    }
  }

  //todo change to Option(Tuple2(... -> ...))
  def toDownload(header: Header): Seq[(ModifierTypeId, ModifierId)] =
  // Already synced and header is not too far back. Download required modifiers
    if (header.height >= blockDownloadProcessor.minimalBlockHeight) Seq(Payload.modifierTypeId -> header.payloadId)
    // Headers chain is synced after this header. Start downloading full blocks
    else if (!isHeadersChainSynced && isNewHeader(header)) {
      isHeadersChainSyncedVar = true
      blockDownloadProcessor.updateBestBlock(header)
      Seq.empty
    } else Seq.empty

  def headerChainBack(limit: Int, startHeader: Header, until: Header => Boolean): HeaderChain = {
    @tailrec def loop(header: Header, acc: Seq[Header]): Seq[Header] = {
      if (acc.length == limit || until(header)) acc
      else getHeaderById(header.parentId) match {
        case Some(parent: Header) => loop(parent, acc :+ parent)
        case None if acc.contains(header) => acc
        case _ => acc :+ header
      }
    }

    if (getBestHeaderId.isEmpty || (limit == 0)) HeaderChain(Seq.empty)
    else HeaderChain(loop(startHeader, Seq(startHeader)).reverse)
  }

  @tailrec final def loopHeightDown(height: Int, p: ModifierId => Boolean): Option[Header] = headerIdsAtHeight(height)
    .find(p)
    .flatMap(getHeaderById) match {
    case h@Some(_) => h
    case None if height > TestNetConstants.GenesisHeight => loopHeightDown(height - 1, p)
    case n@None => n
  }

  def requiredDifficultyAfter(parent: Header): Either[ValidationError, Difficulty] = {
    val requiredHeights: Seq[Height] = PowLinearController.getHeightsForRetargetingAt(Height @@ (parent.height + 1))
      .ensuring(_.last == parent.height, "Incorrect heights sequence!")
    for {
      _ <- Either.cond(requiredHeights.lastOption.contains(parent.height), (),
        HistoryExternalApiError("Incorrect heights sequence in requiredDifficultyAfter function"))
      chain = headerChainBack(requiredHeights.max - requiredHeights.min + 1, parent, (_: Header) => false)
      requiredHeaders = (requiredHeights.min to requiredHeights.max)
        .zip(chain.headers)
        .filter(p => requiredHeights.contains(p._1))
      _ <- Either.cond(requiredHeights.length == requiredHeaders.length, (),
        HistoryExternalApiError(s"Missed headers: $requiredHeights != ${requiredHeaders.map(_._1)}"))
    } yield PowLinearController.getDifficulty(requiredHeaders)
  }


  def isHeadersChainSynced: Boolean = isHeadersChainSyncedVar


  def addHeaderToCacheIfNecessary(h: Header): Unit =
    if (h.height >= getBestHeaderHeight - TestNetConstants.MaxRollbackDepth) {
      logger.debug(s"Should add ${Algos.encode(h.id)} to header cache")
      val newHeadersIdsAtHeaderHeight = headersCacheIndexes.getOrElse(h.height, Seq.empty[ModifierId]) :+ h.id
      headersCacheIndexes = headersCacheIndexes + (h.height -> newHeadersIdsAtHeaderHeight)
      headersCache = headersCache + (ByteArrayWrapper(h.id) -> h)
      // cleanup cache if necessary
      if (headersCacheIndexes.size > TestNetConstants.MaxRollbackDepth) {
        headersCacheIndexes.get(getBestHeaderHeight - TestNetConstants.MaxRollbackDepth).foreach { headersIds =>
          val wrappedIds = headersIds.map(ByteArrayWrapper.apply)
          logger.debug(s"Cleanup header cache from headers: ${headersIds.map(Algos.encode).mkString(",")}")
          headersCache = headersCache.filterNot { case (id, _) => wrappedIds.contains(id) }
        }
        headersCacheIndexes = headersCacheIndexes - (getBestHeaderHeight - TestNetConstants.MaxRollbackDepth)
      }
      logger.debug(s"headersCache size: ${headersCache.size}")
      logger.debug(s"headersCacheIndexes size: ${headersCacheIndexes.size}")
    }

  def addBlockToCacheIfNecessary(b: Block): Unit =
    if (b.header.height >= getBestBlockHeight - TestNetConstants.MaxRollbackDepth) {
      logger.debug(s"Should add ${Algos.encode(b.id)} to header cache")
      val newBlocksIdsAtBlockHeight = blocksCacheIndexes.getOrElse(b.header.height, Seq.empty[ModifierId]) :+ b.id
      blocksCacheIndexes = blocksCacheIndexes + (b.header.height -> newBlocksIdsAtBlockHeight)
      blocksCache = blocksCache + (ByteArrayWrapper(b.id) -> b)
      // cleanup cache if necessary
      if (blocksCacheIndexes.size > TestNetConstants.MaxRollbackDepth) {
        blocksCacheIndexes.get(getBestBlockHeight - TestNetConstants.MaxRollbackDepth).foreach { blocksIds =>
          val wrappedIds = blocksIds.map(ByteArrayWrapper.apply)
          logger.debug(s"Cleanup block cache from headers: ${blocksIds.map(Algos.encode).mkString(",")}")
          blocksCache = blocksCache.filterNot { case (id, _) => wrappedIds.contains(id) }
        }
        blocksCacheIndexes = blocksCacheIndexes - (getBestBlockHeight - TestNetConstants.MaxRollbackDepth)
      }
      logger.debug(s"headersCache size: ${blocksCache.size}")
      logger.debug(s"headersCacheIndexes size: ${blocksCacheIndexes.size}")
    }


  def syncInfo: SyncInfo =
    if (getBestHeaderId.isEmpty) SyncInfo(Seq.empty)
    else SyncInfo(getBestHeader.map(header =>
      ((header.height - settings.network.maxInvObjects + 1) to header.height)
        .flatMap(height => headerIdsAtHeight(height).headOption)
    ).getOrElse(Seq.empty))


  def compare(si: SyncInfo): HistoryComparisonResult = getBestHeaderId match {

    //Our best header is the same as other history best header
    case Some(id) if si.lastHeaderIds.lastOption.exists(_ sameElements id) => Equal

    //Our best header is in other history best chain, but not at the last position
    case Some(id) if si.lastHeaderIds.exists(_ sameElements id) => Older

    /* Other history is empty, or our history contains last id from other history */
    case Some(_) if si.lastHeaderIds.isEmpty || si.lastHeaderIds.lastOption.exists(isHeaderDefined) => Younger

    case Some(_) =>
      //Our history contains some ids from other history
      if (si.lastHeaderIds.exists(isHeaderDefined)) Fork
      //Unknown comparison result
      else Unknown

    //Both nodes do not keep any blocks
    case None if si.lastHeaderIds.isEmpty => Equal

    //Our history is empty, other contain some headers
    case None => Older
  }

  def continuationIds(info: SyncInfo, size: Int): Option[ModifierIds] = Try {
    if (getBestHeaderId.isEmpty) info.startingPoints
    else if (info.lastHeaderIds.isEmpty) {
      val heightFrom: Int = Math.min(getBestHeaderHeight, size - 1)
      val startId: ModifierId = headerIdsAtHeight(heightFrom).head
      //todo remove .get
      val startHeader: Header = getHeaderById(startId).get
      val headers: HeaderChain = headerChainBack(size, startHeader, _ => false)
        .ensuring(_.headers.exists(_.height == TestNetConstants.GenesisHeight),
          "Should always contain genesis header.")
      headers.headers.flatMap(h => Seq((Header.modifierTypeId, h.id)))
    } else {
      val ids: Seq[ModifierId] = info.lastHeaderIds
      val lastHeaderInOurBestChain: ModifierId = ids.view.reverse.find(m => isInBestChain(m)).get
      val theirHeight: Height = heightOf(lastHeaderInOurBestChain).get
      val heightFrom: Int = Math.min(getBestHeaderHeight, theirHeight + size)
      val startId: ModifierId = headerIdsAtHeight(heightFrom).head
      //todo remove .get
      val startHeader: Header = getHeaderById(startId).get
      headerChainBack(size, startHeader, h => h.parentId sameElements lastHeaderInOurBestChain)
        .headers.map(h => Header.modifierTypeId -> h.id)
    }
  }.toOption

  def lastHeaders(count: Int): HeaderChain = getBestHeader
    .map(bestHeader => headerChainBack(count, bestHeader, _ => false))
    .getOrElse(HeaderChain.empty)

  def modifierBytesById(id: ModifierId): Option[Array[Byte]] = historyStorage.modifiersBytesById(id)

  def commonBlockThenSuffixes(header1: Header,
                              header2: Header): (HeaderChain, HeaderChain) = {
    val heightDelta: Int = Math.max(header1.height - header2.height, 0)

    def loop(numberBack: Int, otherChain: HeaderChain): (HeaderChain, HeaderChain) = {
      val chains: (HeaderChain, HeaderChain) = commonBlockThenSuffixes(otherChain, header1, numberBack + heightDelta)
      if (chains._1.head == chains._2.head) chains
      else {
        val biggerOther: HeaderChain = headerChainBack(numberBack, otherChain.head, _ => false) ++ otherChain.tail
        if (!otherChain.head.isGenesis) loop(biggerOther.length, biggerOther)
        else throw new Exception(s"Common point not found for headers $header1 and $header2")
      }
    }

    def commonBlockThenSuffixes(otherChain: HeaderChain,
                                startHeader: Header,
                                limit: Int): (HeaderChain, HeaderChain) = {
      def until(h: Header): Boolean = otherChain.exists(_.id sameElements h.id)

      val currentChain: HeaderChain = headerChainBack(limit, startHeader, until)
      (currentChain, otherChain.takeAfter(currentChain.head))
    }

    loop(2, HeaderChain(Seq(header2)))
  }

  def getChainToHeader(fromHeaderOpt: Option[Header],
                       toHeader: Header): (Option[ModifierId], HeaderChain) = fromHeaderOpt match {
    case Some(h1) =>
      val (prevChain, newChain) = commonBlockThenSuffixes(h1, toHeader)
      (prevChain.headOption.map(_.id), newChain.tail)
    case None => (None, headerChainBack(toHeader.height + 1, toHeader, _ => false))
  }

  def getHeaderIds(count: Int, offset: Int = 0): Seq[ModifierId] = (offset until (count + offset))
    .flatMap(h => headerIdsAtHeight(h).headOption)

  private def isNewHeader(header: Header): Boolean =
    timeProvider.estimatedTime - header.timestamp <
      TestNetConstants.DesiredBlockInterval.toMillis * TestNetConstants.NewHeaderTimeMultiplier
}