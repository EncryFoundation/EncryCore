package encry.view.history

import cats.syntax.option._
import encry.consensus.HistoryConsensus._
import encry.consensus._
import encry.modifiers.history._
import encry.settings.EncryAppSettings
import encry.utils.NetworkTimeProvider
import encry.view.history.ValidationError.HistoryApiError
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.modifiers.history._
import org.encryfoundation.common.network.SyncInfo
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{Difficulty, Height, ModifierId, ModifierTypeId}
import scala.annotation.tailrec
import scala.collection.immutable.HashSet

trait HistoryApi extends HistoryDBApi { //scalastyle:ignore

  val timeProvider: NetworkTimeProvider

  var headersCacheIndexes: Map[Int, Seq[ModifierId]] = Map.empty[Int, Seq[ModifierId]]

  var headersCache: Map[ByteArrayWrapper, Header] = Map.empty[ByteArrayWrapper, Header]

  var blocksCacheIndexes: Map[Int, Seq[ModifierId]] = Map.empty[Int, Seq[ModifierId]]

  var blocksCache: Map[ByteArrayWrapper, Block] = Map.empty[ByteArrayWrapper, Block]

  private var lastSyncInfo: SyncInfo = SyncInfo(Seq.empty[ModifierId])

  lazy val blockDownloadProcessor: BlockDownloadProcessor = BlockDownloadProcessor(settings.node, settings.constants)

  var isHeadersChainSyncedVar: Boolean = false

  final case class FastSyncProcessor(localSettings: EncryAppSettings) {
    var fastSyncVal: Boolean = settings.snapshotSettings.enableFastSynchronization && !settings.node.offlineGeneration
  }

  var lastAvailableManifestHeight: Int = 0

  lazy val fastSyncInProgress: FastSyncProcessor = FastSyncProcessor(settings)

  def getHeaderById(id: ModifierId): Option[Header] = headersCache
    .get(ByteArrayWrapper(id))
    .orElse(blocksCache.get(ByteArrayWrapper(id)).map(_.header))
    .orElse(getHeaderByIdDB(id))

  def getBlockByHeaderId(id: ModifierId): Option[Block] =
    blocksCache
    .get(ByteArrayWrapper(id))
    .orElse(headersCache.get(ByteArrayWrapper(id))
    .flatMap(h => getPayloadByIdDB(h.payloadId).map(p => Block(h, p))))
    .orElse(getBlockByHeaderIdDB(id))

  def getBlockByHeader(header: Header): Option[Block] = blocksCache
    .get(ByteArrayWrapper(header.id))
    .orElse(getPayloadByIdDB(header.payloadId).map(p => Block(header, p)))

  def getBestHeader: Option[Header] = getBestHeaderId.flatMap(id =>
    headersCache
      .get(ByteArrayWrapper(id))
      .orElse(blocksCache.get(ByteArrayWrapper(id)).map(_.header))
      .orElse(getHeaderByIdDB(id))
  )

  def getBestHeaderHeight: Int = getBestHeaderId.flatMap(id =>
    headersCache.get(ByteArrayWrapper(id)).map(_.height)
      .orElse(blocksCache.get(ByteArrayWrapper(id)).map(_.header.height))
      .orElse(getHeightByHeaderId(id))
  ).getOrElse(settings.constants.PreGenesisHeight)

  def getBestBlock: Option[Block] = getBestBlockId.flatMap(id =>
    blocksCache.get(ByteArrayWrapper(id))
      .orElse(getBlockByHeaderIdDB(id))
  )

  def getBestBlockHeight: Int = getBestBlockId
    .flatMap(id => blocksCache.get(ByteArrayWrapper(id)).map(_.header.height).orElse(getHeightByHeaderId(id)))
    .getOrElse(settings.constants.PreGenesisHeight)

  def getHeaderOfBestBlock: Option[Header] = getBestBlockId.flatMap(id =>
    headersCache.get(ByteArrayWrapper(id))
      .orElse(blocksCache.get(ByteArrayWrapper(id)).map(_.header))
      .orElse(getHeaderByIdDB(id))
  )

  def getBestHeaderAtHeight(h: Int): Option[Header] = getBestHeaderAtHeightDB(h)

  def getBlockByPayload(payload: Payload): Option[Block] = headersCache
    .get(ByteArrayWrapper(payload.headerId)).map(h => Block(h, payload))
    .orElse(blocksCache.get(ByteArrayWrapper(payload.headerId)))
    .orElse(getHeaderById(payload.headerId).flatMap(h => Some(Block(h, payload))))

  def getHeightByHeaderId(id: ModifierId): Option[Int] = headersCache
    .get(ByteArrayWrapper(id)).map(_.height)
    .orElse(blocksCache.get(ByteArrayWrapper(id)).map(_.header.height))
    .orElse(getHeightByHeaderIdDB(id))

  def isBestBlockDefined: Boolean =
    getBestBlockId.map(id => blocksCache.contains(ByteArrayWrapper(id))).isDefined ||
      getHeaderOfBestBlock.map(h => isModifierDefined(h.payloadId)).isDefined

  def isBlockDefined(header: Header): Boolean =
    blocksCache.get(ByteArrayWrapper(header.id)).isDefined || isModifierDefined(header.payloadId)

  def isHeaderDefined(id: ModifierId): Boolean =
    headersCache.get(ByteArrayWrapper(id)).isDefined ||
      blocksCache.get(ByteArrayWrapper(id)).isDefined ||
      isModifierDefined(id)

  def getBestHeaderIdAtHeight(h: Int): Option[ModifierId] = getBestHeaderIdAtHeightDB(h)
  def headerIdsAtHeight(height: Int): Seq[ModifierId] = headerIdsAtHeightDB(height)
    .getOrElse(Seq.empty[ModifierId])

  def modifierBytesById(id: ModifierId): Option[Array[Byte]] = headersCache
    .get(ByteArrayWrapper(id)).map(h => HeaderProtoSerializer.toProto(h).toByteArray)
    .orElse(blocksCache.get(ByteArrayWrapper(id)).map(b => BlockProtoSerializer.toProto(b).toByteArray))
    .orElse(modifierBytesByIdDB(id))

  def lastHeaders(count: Int): HeaderChain = getBestHeader
    .map(bestHeader => headerChainBack(count, bestHeader, _ => false))
    .getOrElse(HeaderChain.empty)

  def getHeaderIds(count: Int, offset: Int = 0): Seq[ModifierId] = (offset until (count + offset))
    .flatMap(getBestHeaderIdAtHeight)

  def payloadsIdsToDownload(howMany: Int, excluding: HashSet[ModifierId]): Seq[ModifierId] = {
    @tailrec def continuation(height: Int, acc: Seq[ModifierId]): Seq[ModifierId] =
      if (acc.lengthCompare(howMany) >= 0) acc
      else if (height > lastAvailableManifestHeight && fastSyncInProgress.fastSyncVal) acc
      else getBestHeaderIdAtHeight(height).flatMap(getHeaderById) match {
        case Some(h) if !excluding.exists(_.sameElements(h.payloadId)) && !isBlockDefined(h) =>
          continuation(height + 1, acc :+ h.payloadId)
        case Some(_) =>
          continuation(height + 1, acc)
        case None =>
          acc
      }

    (for {
      bestBlockId             <- getBestBlockId
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

  def toDownload(header: Header): Option[(ModifierTypeId, ModifierId)] =
    // Already synced and header is not too far back. Download required modifiers
    if (header.height >= blockDownloadProcessor.minimalBlockHeight) (Payload.modifierTypeId -> header.payloadId).some
    // Headers chain is synced after this header. Start downloading full blocks
    else if (!isHeadersChainSynced && isNewHeader(header)) {
      isHeadersChainSyncedVar = true
      blockDownloadProcessor.updateBestBlock(header)
      none
    } else none

  def headerChainBack(limit: Int, startHeader: Header, until: Header => Boolean): HeaderChain = {
    @tailrec def loop(header: Header, acc: Seq[Header]): Seq[Header] = {
      if (acc.length == limit || until(header)) acc
      else getHeaderById(header.parentId) match {
        case Some(parent: Header)         => loop(parent, acc :+ parent)
        case None if acc.contains(header) => acc
        case _                            => acc :+ header
      }
    }

    if (getBestHeaderId.isEmpty || (limit == 0)) HeaderChain(Seq.empty)
    else HeaderChain(loop(startHeader, Seq(startHeader)).reverse)
  }

  @tailrec final def loopHeightDown(height: Int, p: ModifierId => Boolean): Option[Header] = headerIdsAtHeight(height)
    .find(p)
    .flatMap(getHeaderById) match {
      case h@Some(_)                                       => h
      case None if height > settings.constants.GenesisHeight => loopHeightDown(height - 1, p)
      case n@None                                          => n
  }

  def requiredDifficultyAfter(parent: Header): Either[HistoryApiError, Difficulty] = {
    val requiredHeights: Seq[Height] = PowLinearController.getHeightsForRetargetingAt(Height @@ (parent.height + 1),
      settings.constants.EpochLength, settings.constants.RetargetingEpochsQty)
    for {
      _ <- Either.cond(requiredHeights.lastOption.contains(parent.height), (),
        HistoryApiError("Incorrect heights sequence in requiredDifficultyAfter function"))
      chain = headerChainBack(requiredHeights.max - requiredHeights.min + 1, parent, (_: Header) => false)
      requiredHeaders = (requiredHeights.min to requiredHeights.max)
        .zip(chain.headers)
        .filter(p => requiredHeights.contains(p._1))
      _ <- Either.cond(requiredHeights.length == requiredHeaders.length, (),
        HistoryApiError(s"Missed headers: $requiredHeights != ${requiredHeaders.map(_._1)}"))
    } yield PowLinearController.getDifficulty(requiredHeaders, settings.constants.EpochLength,
      settings.constants.DesiredBlockInterval, settings.constants.InitialDifficulty)
  }

  def syncInfo: SyncInfo = lastSyncInfo

  def updateIdsForSyncInfo(): Unit =
    lastSyncInfo = SyncInfo(getBestHeader.map { header: Header =>
      ((header.height - settings.network.maxInvObjects + 1) to header.height).flatMap { height: Int =>
        headerIdsAtHeight(height).headOption
      }.toList
    }.getOrElse(List.empty))

  def compare(si: SyncInfo): HistoryComparisonResult = lastSyncInfo.lastHeaderIds.lastOption match {
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

  def continuationIds(info: SyncInfo, size: Int): Seq[ModifierId] =
    if (getBestHeaderId.isEmpty) info.startingPoints.map(_._2)
    else if (info.lastHeaderIds.isEmpty) {
      val heightFrom: Int = Math.min(getBestHeaderHeight, size - 1)
      (for {
        startId     <- headerIdsAtHeight(heightFrom).headOption
        startHeader <- getHeaderById(startId)
      } yield headerChainBack(size, startHeader, _ => false)) match {
        case Some(value) if value.headers.exists(_.height == settings.constants.GenesisHeight) => value.headers.map(_.id)
        case _ => Seq.empty
      }
    } else {
      val ids: Seq[ModifierId] = info.lastHeaderIds
      (for {
        lastHeaderInOurBestChain <- ids.view.reverse.find(m => isInBestChain(m))
        theirHeight              <- heightOf(lastHeaderInOurBestChain)
        heightFrom = Math.min(getBestHeaderHeight, theirHeight + size)
        startId                  <- headerIdsAtHeight(heightFrom).headOption
        startHeader              <- getHeaderById(startId)
      } yield headerChainBack(size, startHeader, h => h.parentId sameElements lastHeaderInOurBestChain)
          .headers
          .map(_.id)) match {
            case Some(value) => value
            case None        => Seq.empty
      }
    }

  def commonBlockThenSuffixes(header1: Header,
                              header2: Header): (HeaderChain, HeaderChain) = {
    val heightDelta: Int = Math.max(header1.height - header2.height, 0)

    @scala.annotation.tailrec
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

  def isNewHeader(header: Header): Boolean =
    timeProvider.estimatedTime - header.timestamp <
      settings.constants.DesiredBlockInterval.toMillis * settings.constants.NewHeaderTimeMultiplier

  def isHeadersChainSynced: Boolean = isHeadersChainSyncedVar

  def continuationHeaderChains(header: Header,
                               filterCond: Header => Boolean): Seq[Seq[Header]] = {
    @tailrec def loop(currentHeight: Int, acc: Seq[Seq[Header]]): Seq[Seq[Header]] = {
      val nextHeightHeaders: Seq[Header] = headerIdsAtHeight(currentHeight + 1)
        .view
        .flatMap(getHeaderById)
        .filter(filterCond)
        .toList
      if (nextHeightHeaders.isEmpty) acc.map(_.reverse)
      else {
        val updatedChains: Seq[Seq[Header]] = nextHeightHeaders.flatMap(h =>
          acc.find(chain => chain.nonEmpty && (h.parentId sameElements chain.head.id)).map(h +: _)
        )
        val nonUpdatedChains: Seq[Seq[Header]] =
          acc.filter(chain => !nextHeightHeaders.exists(_.parentId sameElements chain.head.id))

        loop(currentHeight + 1, updatedChains ++ nonUpdatedChains)
      }
    }

    loop(header.height, Seq(Seq(header)))
  }

  def addHeaderToCacheIfNecessary(h: Header): Unit =
    if (h.height >= getBestHeaderHeight - settings.constants.MaxRollbackDepth) {
      logger.debug(s"Should add ${Algos.encode(h.id)} to header cache")
      val newHeadersIdsAtHeaderHeight = headersCacheIndexes.getOrElse(h.height, Seq.empty[ModifierId]) :+ h.id
      headersCacheIndexes = headersCacheIndexes + (h.height -> newHeadersIdsAtHeaderHeight)
      headersCache = headersCache + (ByteArrayWrapper(h.id) -> h)
      // cleanup cache if necessary
      if (headersCacheIndexes.size > settings.constants.MaxRollbackDepth) {
        headersCacheIndexes.get(getBestHeaderHeight - settings.constants.MaxRollbackDepth).foreach { headersIds =>
          val wrappedIds = headersIds.map(ByteArrayWrapper.apply)
          logger.debug(s"Cleanup header cache from headers: ${headersIds.map(Algos.encode).mkString(",")}")
          headersCache = headersCache.filterNot { case (id, _) => wrappedIds.contains(id) }
        }
        headersCacheIndexes = headersCacheIndexes - (getBestHeaderHeight - settings.constants.MaxRollbackDepth)
      }
      logger.debug(s"headersCache size: ${headersCache.size}")
      logger.debug(s"headersCacheIndexes size: ${headersCacheIndexes.size}")
    }

  def addBlockToCacheIfNecessary(b: Block): Unit =
    if (!blocksCache.contains(ByteArrayWrapper(b.id)) && (b.header.height >= getBestBlockHeight - settings.constants.MaxRollbackDepth)) {
      logger.debug(s"Should add ${Algos.encode(b.id)} to block cache")
      val newBlocksIdsAtBlockHeight = blocksCacheIndexes.getOrElse(b.header.height, Seq.empty[ModifierId]) :+ b.id
      blocksCacheIndexes = blocksCacheIndexes + (b.header.height -> newBlocksIdsAtBlockHeight)
      blocksCache = blocksCache + (ByteArrayWrapper(b.id) -> b)
      // cleanup cache if necessary
      if (blocksCacheIndexes.size > settings.constants.MaxRollbackDepth) {
        blocksCacheIndexes.get(getBestBlockHeight - settings.constants.MaxRollbackDepth).foreach { blocksIds =>
          val wrappedIds = blocksIds.map(ByteArrayWrapper.apply)
          logger.debug(s"Cleanup block cache from headers: ${blocksIds.map(Algos.encode).mkString(",")}")
          blocksCache = blocksCache.filterNot { case (id, _) => wrappedIds.contains(id) }
        }
        blocksCacheIndexes = blocksCacheIndexes - (getBestBlockHeight - settings.constants.MaxRollbackDepth)
      }
      logger.debug(s"headersCache size: ${blocksCache.size}")
      logger.debug(s"headersCacheIndexes size: ${blocksCacheIndexes.size}")
    }
}