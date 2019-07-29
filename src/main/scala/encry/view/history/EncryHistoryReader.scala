package encry.view.history

import com.typesafe.scalalogging.StrictLogging
import encry.consensus.History._
import encry.modifiers.history._
import encry.settings.NodeSettings
import encry.view.history.ValidationError.FatalValidationError.UnknownModifierFatalError
import encry.view.history.processors.BlockHeaderProcessor
import encry.view.history.processors.payload.BlockPayloadProcessor
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{Block, Header, Payload}
import org.encryfoundation.common.network.SyncInfo
import org.encryfoundation.common.utils.TaggedTypes.{Height, ModifierId, ModifierTypeId}
import org.encryfoundation.common.utils.constants.TestNetConstants
import org.encryfoundation.common.validation.ModifierSemanticValidity
import cats.syntax.either._

import scala.annotation.tailrec
import scala.util.Try

trait EncryHistoryReader extends BlockHeaderProcessor
  with BlockPayloadProcessor
  with StrictLogging {

  protected val nodeSettings: NodeSettings

  /** Is there's no history, even genesis block */
//  def isEmpty: Boolean = bestHeaderIdOpt.isEmpty
//
//  def contains(id: ModifierId): Boolean = historyStorage.containsMod(id)

  /**
    * Complete block of the best chain with transactions.
    * Always None for an SPV mode, Some(fullBLock) for full node regime after initial bootstrap.
    */
//  def bestBlockOpt: Option[Block] = bestBlockIdOpt.flatMap(id => getBlock(id))

  def headerOfBestBlock: Option[Header] =
    bestBlockIdOpt.flatMap(id => blocksCache.get(ByteArrayWrapper(id)).map(_.header).orElse(typedModifierById[Header](id)))

//  def getHeaderById(id: ModifierId): Option[Header] = lastAppliedHeadersCache
//    .get(ByteArrayWrapper(id))
//    .orElse(blocksCache.get(ByteArrayWrapper(id)).map(_.header))
//    .orElse(typedModifierById[Header](id))

//  def isBlockDefined(header: Header): Boolean =
//    blocksCache.get(ByteArrayWrapper(header.id)).isDefined || isModifierDefined(header.payloadId)

//  /** @return ids of count headers starting from offset */
//  def getHeaderIds(count: Int, offset: Int = 0): Seq[ModifierId] = (offset until (count + offset))
//    .flatMap(h => headerIdsAtHeight(h).headOption)

//  /**
//    * Whether another's node syncInfo shows that another node is ahead or behind ours
//    *
//    * @param si other's node sync info
//    * @return Equal if nodes have the same history, Younger if another node is behind, Older if a new node is ahead
//    */
//  def compare(si: SyncInfo): HistoryComparisonResult = bestHeaderIdOpt match {
//
//    //Our best header is the same as other history best header
//    case Some(id) if si.lastHeaderIds.lastOption.exists(_ sameElements id) => Equal
//
//    //Our best header is in other history best chain, but not at the last position
//    case Some(id) if si.lastHeaderIds.exists(_ sameElements id) => Older
//
//    /* Other history is empty, or our history contains last id from other history */
//    case Some(_) if si.lastHeaderIds.isEmpty || si.lastHeaderIds.lastOption.exists(isModifierDefined) => Younger
//
//    case Some(_) =>
//      //Our history contains some ids from other history
//      if (si.lastHeaderIds.exists(isModifierDefined)) Fork
//      //Unknown comparison result
//      else Unknown
//
//    //Both nodes do not keep any blocks
//    case None if si.lastHeaderIds.isEmpty => Equal
//
//    //Our history is empty, other contain some headers
//    case None => Older
//  }

//  def continuationIds(info: SyncInfo, size: Int): Option[ModifierIds] = Try {
//    if (isEmpty) info.startingPoints
//    else if (info.lastHeaderIds.isEmpty) {
//      val heightFrom: Int = Math.min(bestHeaderHeight, size - 1)
//      val startId: ModifierId = headerIdsAtHeight(heightFrom).head
//      //todo remove .get
//      val startHeader: Header = getHeaderById(startId).get
//      val headers: HeaderChain = headerChainBack(size, startHeader, _ => false)
//        .ensuring(_.headers.exists(_.height == TestNetConstants.GenesisHeight),
//          "Should always contain genesis header.")
//      headers.headers.flatMap(h => Seq((Header.modifierTypeId, h.id)))
//    } else {
//      val ids: Seq[ModifierId] = info.lastHeaderIds
//      val lastHeaderInOurBestChain: ModifierId = ids.view.reverse.find(m => isInBestChain(m)).get
//      val theirHeight: Height = heightOf(lastHeaderInOurBestChain).get
//      val heightFrom: Int = Math.min(bestHeaderHeight, theirHeight + size)
//      val startId: ModifierId = headerIdsAtHeight(heightFrom).head
//      //todo remove .get
//      val startHeader: Header = getHeaderById(startId).get
//      headerChainBack(size, startHeader, h => h.parentId sameElements lastHeaderInOurBestChain)
//        .headers.map(h => Header.modifierTypeId -> h.id)
//    }
//  }.toOption

//  /** @return all possible forks, that contains specified header */
//  protected[history] def continuationHeaderChains(header: Header,
//                                                  filterCond: Header => Boolean): Seq[Seq[Header]] = {
//    @tailrec
//    def loop(currentHeight: Int, acc: Seq[Seq[Header]]): Seq[Seq[Header]] = {
//      val nextLevelHeaders: Seq[Header] = Seq(currentHeight)
//        .flatMap(h => headerIdsAtHeight(h + 1))
//        .flatMap(getHeaderById)
//        .filter(filterCond)
//      if (nextLevelHeaders.isEmpty) acc.map(_.reverse)
//      else {
//        val updatedChains: Seq[Seq[Header]] = nextLevelHeaders.flatMap { h =>
//          acc.find(chain => chain.nonEmpty && (h.parentId sameElements chain.head.id)).map(h +: _)
//        }
//        val nonUpdatedChains: Seq[Seq[Header]] = acc.filter(chain =>
//          !nextLevelHeaders.exists(_.parentId sameElements chain.head.id))
//        loop(currentHeight + 1, updatedChains ++ nonUpdatedChains)
//      }
//    }
//
//    loop(header.height, Seq(Seq(header)))
//  }

//  def testApplicable(modifier: PersistentModifier): Either[ValidationError, PersistentModifier] = {
//    val validationResult: Either[ValidationError, PersistentModifier] = modifier match {
//      case header: Header => validate(header)
//      case payload: Payload => validate(payload)
//      case mod => UnknownModifierFatalError(s"Modifier $mod has incorrect type.").asLeft[PersistentModifier]
//    }
//    validationResult match {
//      case Left(value) => logger.info(s"Validation result failed: $value"); validationResult
//      case Right(m) => logger.info(s"Validation result successful for ${m.encodedId}"); validationResult
//    }
//  }

//  def lastHeaders(count: Int): HeaderChain = bestHeaderOpt
//    .map(bestHeader => headerChainBack(count, bestHeader, _ => false))
//    .getOrElse(HeaderChain.empty)

//  def modifierById(id: ModifierId): Option[PersistentModifier] = historyStorage.modifierById(id)
//
//  def modifierBytesById(id: ModifierId): Option[Array[Byte]] = historyStorage.modifiersBytesById(id)

//  def typedModifierById[T <: PersistentModifier](id: ModifierId): Option[T] = modifierById(id) match {
//    case Some(m: T@unchecked) if m.isInstanceOf[T] => Some(m)
//    case _ => None
//  }

//  def getBlock(header: Header): Option[Block] = blocksCache
//    .get(ByteArrayWrapper(header.id))
//    .orElse(typedModifierById[Payload](header.payloadId).map(payload => Block(header, payload)))
//
//  def getBlock(id: ModifierId): Option[Block] = blocksCache
//    .get(ByteArrayWrapper(id))
//    .orElse(typedModifierById[Header](id).flatMap(h => typedModifierById[Payload](h.payloadId).map(p => Block(h, p))))

//  /**
//    * Return headers, required to apply to reach header2 if you are at header1 position.
//    *
//    * @param fromHeaderOpt - initial position
//    * @param toHeader      - header you should reach
//    * @return (Modifier required to rollback first, header chain to apply)
//    */
//  def getChainToHeader(fromHeaderOpt: Option[Header],
//                       toHeader: Header): (Option[ModifierId], HeaderChain) = fromHeaderOpt match {
//    case Some(h1) =>
//      val (prevChain, newChain) = commonBlockThenSuffixes(h1, toHeader)
//      (prevChain.headOption.map(_.id), newChain.tail)
//    case None => (None, headerChainBack(toHeader.height + 1, toHeader, _ => false))
//  }
//
//  /** Finds common block and sub-chains from common block to header1 and header2. */
//  protected[history] def commonBlockThenSuffixes(header1: Header,
//                                                 header2: Header): (HeaderChain, HeaderChain) = {
//    val heightDelta: Int = Math.max(header1.height - header2.height, 0)
//
//    def loop(numberBack: Int, otherChain: HeaderChain): (HeaderChain, HeaderChain) = {
//      val chains: (HeaderChain, HeaderChain) = commonBlockThenSuffixes(otherChain, header1, numberBack + heightDelta)
//      if (chains._1.head == chains._2.head) chains
//      else {
//        val biggerOther: HeaderChain = headerChainBack(numberBack, otherChain.head, _ => false) ++ otherChain.tail
//        if (!otherChain.head.isGenesis) loop(biggerOther.length, biggerOther)
//        else throw new Exception(s"Common point not found for headers $header1 and $header2")
//      }
//    }
//
//    loop(2, HeaderChain(Seq(header2)))
//  }
//
//  /** Finds common block and sub-chains with `otherChain`. */
//  protected[history] def commonBlockThenSuffixes(otherChain: HeaderChain,
//                                                 startHeader: Header,
//                                                 limit: Int): (HeaderChain, HeaderChain) = {
//    def until(h: Header): Boolean = otherChain.exists(_.id sameElements h.id)
//
//    val currentChain: HeaderChain = headerChainBack(limit, startHeader, until)
//    (currentChain, otherChain.takeAfter(currentChain.head))
//  }
//
//  def syncInfo: SyncInfo =
//    if (isEmpty) SyncInfo(Seq.empty)
//    else SyncInfo(bestHeaderOpt.map(header =>
//      ((header.height - settings.network.maxInvObjects + 1) to header.height)
//        .flatMap(height => headerIdsAtHeight(height).headOption)
//    ).getOrElse(Seq.empty))

//  override def isSemanticallyValid(modifierId: ModifierId): ModifierSemanticValidity =
//    historyStorage.store.get(validityKey(modifierId)) match {
//      case Some(mod) if mod.headOption.contains(1.toByte) => ModifierSemanticValidity.Valid
//      case Some(mod) if mod.headOption.contains(0.toByte) => ModifierSemanticValidity.Invalid
//      case None if isModifierDefined(modifierId) => ModifierSemanticValidity.Unknown
//      case None => ModifierSemanticValidity.Absent
//      case mod => logger.error(s"Incorrect validity status: $mod")
//        ModifierSemanticValidity.Absent
//    }
}