package encry.view.history

import java.io.File
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.History.ProgressInfo
import encry.modifiers.history.HeaderChain
import encry.settings.EncryAppSettings
import encry.storage.VersionalStorage
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.storage.iodb.versionalIODB.IODBHistoryWrapper
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
import encry.utils.NetworkTimeProvider
import cats.syntax.either._
import encry.view.history.storage.HistoryStorage
import io.iohk.iodb.LSMStore
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{Block, Header, Payload}
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
import org.iq80.leveldb.Options
import cats.syntax.option._
import encry.view.history.HistoryValidationError.HistoryExtensionError

/**
  * History implementation. It is processing persistent modifiers generated locally or received from the network.
  */

final case class EncryHistory(override val history: HistoryStorage, override val settings: EncryAppSettings)
  extends HistoryModifiersValidations with HistoryModifiersProcessor with AutoCloseable {

  def isFullChainSynced: Boolean = getBestHeaderIdOpt
    .exists(bestHeaderId => getBestBlockIdOpt.exists(_ sameElements bestHeaderId))

  /** Appends modifier to the history if it is applicable. */
  def append(modifier: PersistentModifier): Either[Throwable, (EncryHistory, ProgressInfo)] = {
    logger.info(s"Trying to append modifier ${Algos.encode(modifier.id)} of type ${modifier.modifierTypeId} to history")
    Either.catchNonFatal(modifier match {
      case header: Header   => this -> process(header)
      case payload: Payload => this -> process(payload)
    })
  }

  def reportModifierIsValid(modifier: PersistentModifier): EncryHistory = {
    logger.info(s"Modifier ${modifier.encodedId} of type ${modifier.modifierTypeId} is marked as valid ")
    markModifierValid(modifier)
    this
  }

  /** Report some modifier as valid or invalid semantically */
  def reportModifierIsInvalid(modifier: PersistentModifier, progressInfo: ProgressInfo): (EncryHistory, ProgressInfo) = {
    logger.info(s"Modifier ${modifier.encodedId} of type ${modifier.modifierTypeId} is marked as invalid")
    this -> markModifierInvalid(modifier)
  }

  /** @return header, that corresponds to modifier */
  private def correspondingHeader(modifier: PersistentModifier): Option[Header] = modifier match {
    case header: Header   => header.some
    case payload: Payload => getHeaderById(payload.headerId)
    case block: Block     => block.header.some
  }

  /**
    * Marks modifier as valid
    *
    * @param modifier that is valid against the State
    * @return ProgressInfo with next modifier to try to apply
    */
  private def markModifierValid(modifier: PersistentModifier): ProgressInfo = modifier match {
    case block: Block =>
      val nonMarkedIds: Seq[ModifierId] = Seq(block.header.id, block.payload.id)
        .filter(id => history.get(validityKey(id)).isEmpty)
      if (nonMarkedIds.nonEmpty) history.insert(
        StorageVersion @@ validityKey(nonMarkedIds.head).untag(StorageKey),
        nonMarkedIds.map(id => validityKey(id) -> StorageValue @@ Array(1.toByte)).toList
      )
      if (getBestBlockIdOpt.exists(_ sameElements block.id))
        // Applies best header to the history
        ProgressInfo(none, Seq.empty, Seq.empty, none)
      else {
        // Marks non-best full block as valid. Should have more blocks to apply to sync state and history.
        val chainBack: HeaderChain = (for {
          bestFullHeader <- getBestBlockOpt.map(_.header)
          limit = bestFullHeader.height - block.header.height
        } yield headerChainBack(limit, bestFullHeader, h => h.parentId sameElements block.header.id))
          .getOrElse(HeaderChain.empty)
        val blockToApply: Option[Block] = chainBack.headOption.flatMap(header => getBlockByHeader(header))
        val toApply: Either[HistoryValidationError, Block] = for {
          _ <- Either.cond(blockToApply.isDefined, (),
            HistoryExtensionError(s"Should have next block to apply, failed for ${block.header}"))
          _ <- Either.cond(blockToApply.exists(_.header.parentId sameElements block.header.id), (),
            HistoryExtensionError(s"Block to apply should link to current block. Failed for ${chainBack.headOption} and ${block.header}"))
        } yield blockToApply.get
        ProgressInfo(none, Seq.empty, toApply.toSeq, none)
      }
    case _ =>
      history.insert(
        StorageVersion @@ validityKey(modifier.id).untag(StorageKey),
        List(validityKey(modifier.id) -> StorageValue @@ Array(1.toByte))
      )
      ProgressInfo(none, Seq.empty, Seq.empty, none)
  }

  /**
    * Marks modifier and all modifiers in child chains as invalid
    *
    * @param modifier that is invalid against the State
    * @return ProgressInfo with next modifier to try to apply
    */
  private def markModifierInvalid(modifier: PersistentModifier): ProgressInfo = correspondingHeader(modifier) match {
    case Some(invalidatedHeader) =>
      val invalidatedHeaders: Seq[Header] = continuationHeaderChains(invalidatedHeader, _ => true).flatten.distinct
      val validityRow: List[(StorageKey, StorageValue)] = invalidatedHeaders
        .flatMap(h => Seq(h.id, h.payloadId).map(id => validityKey(id) -> StorageValue @@ Array(0.toByte)))
        .toList
      logger.info(s"Going to invalidate ${invalidatedHeader.encodedId} and ${invalidatedHeaders.map(_.encodedId)}")
      val bestHeaderIsInvalidated: Boolean = getBestHeaderIdOpt.exists(id => invalidatedHeaders.exists(_.id sameElements id))
      val bestFullIsInvalidated: Boolean = getBestBlockIdOpt.exists(id => invalidatedHeaders.exists(_.id sameElements id))
      (bestHeaderIsInvalidated, bestFullIsInvalidated) match {
        case (false, false) =>
          // Modifiers from best header and best full chain are not involved, no rollback and links change required.
          history.insert(StorageVersion @@ validityKey(modifier.id), validityRow)
          ProgressInfo(none, Seq.empty, Seq.empty, none)
        case _ =>
          // Modifiers from best header and best full chain are involved, links change required.
          val newBestHeaderOpt: Option[Header] =
            loopHeightDown(getBestHeaderHeight, id => !invalidatedHeaders.exists(_.id sameElements id))

          if (!bestFullIsInvalidated) {
            // Only headers chain involved
            for {
              newBestHeader <- newBestHeaderOpt
            } history.insert(
              StorageVersion @@ validityKey(modifier.id).untag(StorageKey),
              List(BestHeaderKey -> StorageValue @@ newBestHeader.id.untag(ModifierId))
            ) //todo check removed ensure
            ProgressInfo(none, Seq.empty, Seq.empty, none)
          } else {
            val invalidatedChain: Seq[Block] = getBestBlockOpt
              .toSeq
              .flatMap(f => headerChainBack(getBestBlockHeight + 1, f.header, h => !invalidatedHeaders.contains(h)).headers)
              .flatMap(h => getBlockByHeader(h))
              //.ensuring(_.lengthCompare(1) > 0, "invalidatedChain should contain at least bestFullBlock and parent")
            //todo add check condition
            val branchPoint: Block = invalidatedChain.head //todo remove .head
            val validChain: Seq[Block] =
              continuationHeaderChains(branchPoint.header, h => getBlockByHeader(h).isDefined && !invalidatedHeaders.contains(h))
                .maxBy(chain => scoreOf(chain.last.id).getOrElse(BigInt(0))) //todo remove .last
                .flatMap(getBlockByHeader)

            val changedLinks: Seq[(StorageKey, StorageValue)] = newBestHeaderOpt.map(h => List(
              BestBlockKey  -> StorageValue @@ validChain.last.id,
              BestHeaderKey -> StorageValue @@ h.id
            )).getOrElse(List.empty)

            val toInsert: List[(StorageKey, StorageValue)] = validityRow ++ changedLinks
            history.insert(StorageVersion @@ validityKey(modifier.id), toInsert)
            ProgressInfo(branchPoint.id.some, invalidatedChain.tail, validChain.tail, none)
          }
      }
    case None =>
      // No headers become invalid. Just mark this particular modifier as invalid.
      history.insert(
        StorageVersion @@ validityKey(modifier.id),
        List(validityKey(modifier.id) -> StorageValue @@ Array(0.toByte))
      )
      ProgressInfo(none, Seq.empty, Seq.empty, none)
  }

  override def close(): Unit = history.close()
}

object EncryHistory extends StrictLogging {

  def getHistoryIndexDir(settings: EncryAppSettings): File = {
    val dir: File = new File(s"${settings.directory}/history/index")
    dir.mkdirs()
    dir
  }

  def getHistoryObjectsDir(settings: EncryAppSettings): File = {
    val dir: File = new File(s"${settings.directory}/history/objects")
    dir.mkdirs()
    dir
  }

  def readOrGenerate(settings: EncryAppSettings, ntp: NetworkTimeProvider): EncryHistory = {
    val historyIndexDir: File = getHistoryIndexDir(settings)
    //Check what kind of storage in settings:
    val vldbInit: VersionalStorage = settings.storage.history match {
      case VersionalStorage.IODB =>
        logger.info("Init history with iodb storage")
        val historyObjectsDir: File = getHistoryObjectsDir(settings)
        val indexStore: LSMStore = new LSMStore(historyIndexDir, keepVersions = 0)
        val objectsStore: LSMStore = new LSMStore(historyObjectsDir, keepVersions = 0)
        IODBHistoryWrapper(indexStore, objectsStore)
      case VersionalStorage.LevelDB =>
        logger.info("Init history with levelDB storage")
        val levelDBInit = LevelDbFactory.factory.open(historyIndexDir, new Options)
        VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB))
    }
    EncryHistory(HistoryStorage(vldbInit), settings)
  }
}