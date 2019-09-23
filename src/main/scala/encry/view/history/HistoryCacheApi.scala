package encry.view.history

import encry.consensus.HistoryConsensus._
import encry.consensus._
import encry.view.history.ValidationError.HistoryApiError
import encry.view.history.ValidationError._
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.modifiers.history._
import org.encryfoundation.common.network.SyncInfo
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{Difficulty, Height, ModifierId}
import scala.annotation.tailrec
import scala.collection.immutable.HashSet
import cats.syntax.either._
import encry.utils.NetworkTimeProvider

trait HistoryCacheApi extends HistoryStorageApi {

  val blockDownloadProcessor: BlockDownloadProcessor
  val isHeaderChainSynced: Boolean
  val timeProvider: NetworkTimeProvider

  var headersCacheIndexes: Map[Int, Seq[ModifierId]] = Map.empty[Int, Seq[ModifierId]]
  var headersCache: Map[ByteArrayWrapper, Header] = Map.empty[ByteArrayWrapper, Header]

  var blocksCacheIndexes: Map[Int, Seq[ModifierId]] = Map.empty[Int, Seq[ModifierId]]
  var blocksCache: Map[ByteArrayWrapper, Block] = Map.empty[ByteArrayWrapper, Block]

  def heightByHeaderOpt(id: ModifierId): Option[Int] = headersCache
    .get(ByteArrayWrapper(id)).map(_.height)
    .orElse(blocksCache.get(ByteArrayWrapper(id)).map(_.header.height))
    .orElse(heightByHeaderStorageApi(id))

  def bestHeaderOpt: Option[Header] = bestHeaderIdStorageApi.flatMap(id =>
    headersCache
      .get(ByteArrayWrapper(id))
      .orElse(blocksCache.get(ByteArrayWrapper(id)).map(_.header))
      .orElse(headerByIdStorageApi(id)))

  def getBestHeaderHeight: Int = bestHeaderIdStorageApi.flatMap(id =>
    headersCache.get(ByteArrayWrapper(id)).map(_.height)
      .orElse(blocksCache.get(ByteArrayWrapper(id)).map(_.header.height))
      .orElse(heightByHeaderOpt(id))
  ).getOrElse(settings.constants.PreGenesisHeight)

  def headerByIdOpt(id: ModifierId): Option[Header] = headersCache
    .get(ByteArrayWrapper(id))
    .orElse(blocksCache.get(ByteArrayWrapper(id)).map(_.header))
    .orElse(headerByIdStorageApi(id))

  def blockByHeaderOpt(header: Header): Option[Block] = blocksCache
    .get(ByteArrayWrapper(header.id))
    .orElse(payloadByIdStorageApi(header.payloadId).map(p => Block(header, p)))

  def bestBlockOpt: Option[Block] = bestBlockIdStorageApi.flatMap(id =>
    blocksCache.get(ByteArrayWrapper(id)).orElse(blockByIdStorageApi(id)))

  def getBestBlockHeight: Int = bestBlockIdStorageApi
    .flatMap(id => blocksCache.get(ByteArrayWrapper(id)).map(_.header.height).orElse(heightByHeaderOpt(id)))
    .getOrElse(settings.constants.PreGenesisHeight)

  def headerOfBestBlockOpt: Option[Header] = bestBlockIdStorageApi.flatMap(id =>
    headersCache.get(ByteArrayWrapper(id))
      .orElse(blocksCache.get(ByteArrayWrapper(id)).map(_.header))
      .orElse(headerByIdStorageApi(id)))

  def blockByPayloadOpt(payload: Payload): Option[Block] = headersCache
    .get(ByteArrayWrapper(payload.headerId)).map(h => Block(h, payload))
    .orElse(blocksCache.get(ByteArrayWrapper(payload.headerId)))
    .orElse(headerByIdOpt(payload.headerId).flatMap(h => Some(Block(h, payload))))

  def isBestBlockDefined: Boolean =
    bestBlockIdStorageApi.map(id => blocksCache.contains(ByteArrayWrapper(id))).isDefined ||
      headerOfBestBlockOpt.map(h => isModifierDefined(h.payloadId)).isDefined

  def isBlockDefined(header: Header): Boolean =
    blocksCache.get(ByteArrayWrapper(header.id)).isDefined || isModifierDefined(header.payloadId)

  def isHeaderDefined(id: ModifierId): Boolean =
    headersCache.get(ByteArrayWrapper(id)).isDefined ||
      blocksCache.get(ByteArrayWrapper(id)).isDefined ||
      isModifierDefined(id)

  def bestHeaderIdAtHeightOpt(h: Int): Option[ModifierId] = headersCacheIndexes.get(h)
    .flatMap(_.headOption)
    .orElse(getBestHeaderIdAtHeightStorageApi(h))

  def headerIdsAtHeight(height: Int): Seq[ModifierId] = headersCacheIndexes
    .getOrElse(height, headerIdsAtHeightStorageApi(height))

  def modifierBytesById(id: ModifierId): Option[Array[Byte]] = headersCache
    .get(ByteArrayWrapper(id)).map(h => HeaderProtoSerializer.toProto(h).toByteArray)
    .orElse(blocksCache.get(ByteArrayWrapper(id)).map(b => BlockProtoSerializer.toProto(b).toByteArray))
    .orElse(modifierBytesByIdStorageApi(id))

  def lastHeaders(count: Int): List[Header] = bestHeaderOpt
    .map(bestHeader => computeForkChain(count, bestHeader, _ => false))
    .getOrElse(List.empty)

  def getHeaderIds(count: Int, offset: Int = 0): Seq[ModifierId] = (offset until (count + offset))
    .flatMap(bestHeaderIdAtHeightOpt)

  def payloadsIdsToDownload(howMany: Int, excluding: HashSet[ModifierId]): Seq[ModifierId] = {
    @tailrec def continuation(height: Int, acc: Seq[ModifierId]): Seq[ModifierId] =
      if (acc.lengthCompare(howMany) >= 0) acc
      else bestHeaderIdAtHeightOpt(height).flatMap(headerByIdOpt) match {
        case Some(h) if !excluding.exists(_.sameElements(h.payloadId)) && !isBlockDefined(h) =>
          continuation(height + 1, acc :+ h.payloadId)
        case Some(_) =>
          continuation(height + 1, acc)
        case None =>
          acc
      }

    (for {
      bestBlockId             <- bestBlockIdStorageApi
      headerLinkedToBestBlock <- headerByIdOpt(bestBlockId)
    } yield headerLinkedToBestBlock) match {
      case _ if !isHeaderChainSynced => Seq.empty
      case Some(header) if isInBestChain(header) => continuation(header.height + 1, Seq.empty)
      case Some(header) =>
        lastBestBlockHeightRelevantToBestChain(header.height)
          .map(height => continuation(height + 1, Seq.empty))
          .getOrElse(continuation(blockDownloadProcessor.minimalBlockHeight, Seq.empty))
      case None => continuation(blockDownloadProcessor.minimalBlockHeight, Seq.empty)
    }
  }

  def computeForkChain(limit: Int, startHeader: Header, until: Header => Boolean): List[Header] = {
    @tailrec def loop(loopHeader: Header, acc: List[Header]): List[Header] =
      if (acc.length == limit || until(loopHeader)) acc
      else headerByIdStorageApi(loopHeader.parentId) match {
        case Some(parent)                     => loop(parent, parent :: acc)
        case None if acc.contains(loopHeader) => acc
        case _                                => loopHeader :: acc
      }

    if (bestHeaderIdStorageApi.isEmpty || (limit == 0)) List.empty[Header]
    else loop(startHeader, List(startHeader))
  }

  @tailrec final def loopHeightDown(height: Int, p: ModifierId => Boolean): Option[Header] = headerIdsAtHeight(height)
    .find(p)
    .flatMap(headerByIdOpt) match {
    case h@Some(_) => h
    case None if height > settings.constants.GenesisHeight => loopHeightDown(height - 1, p)
    case n@None => n
  }

  def requiredDifficultyAfter(parent: Header): Either[HistoryApiError, Difficulty] = {
    val requiredHeights: Seq[Height] = PowLinearController.getHeightsForRetargetingAt(Height @@ (parent.height + 1),
      settings.constants.EpochLength, settings.constants.RetargetingEpochsQty)
    for {
      _ <- Either.cond(requiredHeights.lastOption.contains(parent.height), (),
        HistoryApiError("Incorrect heights sequence in requiredDifficultyAfter function"))
      chain = computeForkChain(requiredHeights.max - requiredHeights.min + 1, parent, (_: Header) => false)
      requiredHeaders = (requiredHeights.min to requiredHeights.max)
        .zip(chain)
        .filter(p => requiredHeights.contains(p._1))
      _ <- Either.cond(requiredHeights.length == requiredHeaders.length, (),
        HistoryApiError(s"Missed headers: $requiredHeights != ${requiredHeaders.map(_._1)}"))
    } yield PowLinearController.getDifficulty(requiredHeaders, settings.constants.EpochLength,
      settings.constants.DesiredBlockInterval, settings.constants.InitialDifficulty)
  }

  def syncInfo: SyncInfo =
    if (bestHeaderIdStorageApi.isEmpty) SyncInfo(Seq.empty)
    else SyncInfo(bestHeaderOpt.map(header =>
      ((header.height - settings.network.maxInvObjects + 1) to header.height)
        .flatMap(height => headerIdsAtHeight(height).headOption)).getOrElse(Seq.empty))


  def compare(si: SyncInfo): HistoryComparisonResult = bestHeaderIdStorageApi match {
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
    if (bestHeaderIdStorageApi.isEmpty) info.startingPoints.map(_._2)
    else if (info.lastHeaderIds.isEmpty) {
      val heightFrom: Int = Math.min(getBestHeaderHeight, size - 1)
      (for {
        startId     <- headerIdsAtHeight(heightFrom).headOption
        startHeader <- headerByIdOpt(startId)
      } yield computeForkChain(size, startHeader, _ => false)) match {
        case Some(value) if value.exists(_.height == settings.constants.GenesisHeight) => value.map(_.id)
        case _ => Seq.empty
      }
    } else {
      val ids: Seq[ModifierId] = info.lastHeaderIds
      (for {
        lastHeaderInOurBestChain <- ids.view.reverse.find(m => isInBestChain(m))
        theirHeight              <- heightOf(lastHeaderInOurBestChain)
        heightFrom = Math.min(getBestHeaderHeight, theirHeight + size)
        startId                  <- headerIdsAtHeight(heightFrom).headOption
        startHeader              <- headerByIdOpt(startId)
      } yield computeForkChain(size, startHeader, h => h.parentId sameElements lastHeaderInOurBestChain).map(_.id)) match {
          case Some(value) => value
          case None        => Seq.empty
      }
    }

  def commonBlockThenSuffixes(firstHeader: Header,
                              secondsHeader: Header): Either[HistoryProcessingError, (List[Header], List[Header])] = {
    val heightDelta: Int = Math.max(firstHeader.height - secondsHeader.height, 0)

    @tailrec def loop(otherChain: List[Header],
                      numberBack: Int): Either[HistoryProcessingError, (List[Header], List[Header])] =
      commonBlocksThenSuffixes(otherChain, firstHeader, numberBack + heightDelta) match {
        case (l1@ ::(head1, _), l2@ ::(head2, _)) if head1 == head2 => (l1 -> l2).asRight[HistoryProcessingError]
        case _ =>
          computeForkChain(numberBack, otherChain.head, _ => false) ::: otherChain.drop(1) match {
            case l@ ::(_, _) if !otherChain.head.isGenesis => loop(l, l.length)
            case _ => HistoryProcessingError("History has no best header after header application.").asLeft
          }
      }

    def commonBlocksThenSuffixes(otherChain: List[Header],
                                 startHeader: Header,
                                 limit: Int): (List[Header], List[Header]) = {
      val until: Header => Boolean = header => otherChain.exists(_.id sameElements header.id)
      val currentChain: List[Header] = computeForkChain(limit, startHeader, until)
      (currentChain, otherChain.dropWhile(_.id sameElements currentChain.head.id)) //todo check dropWhile correctness
    }

    loop(List(secondsHeader), 2) //todo why 2?
  }

  def getChainToHeader(fromHeaderOpt: Option[Header],
                       toHeader: Header): Either[HistoryProcessingError, (Option[ModifierId], List[Header])] =
    fromHeaderOpt match {
      case Some(h1) =>
        commonBlockThenSuffixes(h1, toHeader) match {
          case Left(value) => value.asLeft
          case Right((prevChain, newChain)) => (prevChain.headOption.map(_.id), newChain.tail).asRight
        }
      case None => (None, computeForkChain(toHeader.height + 1, toHeader, _ => false)).asRight
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
    if (b.header.height >= getBestBlockHeight - settings.constants.MaxRollbackDepth) {
      logger.debug(s"Should add ${Algos.encode(b.id)} to header cache")
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