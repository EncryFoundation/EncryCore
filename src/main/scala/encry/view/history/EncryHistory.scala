package encry.view.history

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.header.{EncryBlockHeader, EncryHeaderChain}
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.view.history.storage.HistoryStorage
import encry.view.history.storage.processors.{BlockHeaderProcessor, BlockPayloadProcessor}
import io.iohk.iodb.Store
import scorex.core.ModifierId
import scorex.core.consensus.History
import scorex.core.consensus.History.HistoryComparisonResult
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58

import scala.util.{Failure, Try}

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
  with BlockHeaderProcessor
  with BlockPayloadProcessor
  with ScorexLogging {

  protected val storage: Store

  override protected lazy val historyStorage: HistoryStorage = new HistoryStorage(storage)

  def bestHeaderOpt: Option[EncryBlockHeader] = bestHeaderIdOpt.flatMap(typedModifierById[EncryBlockHeader])

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

  override def append(modifier: EncryPersistentModifier): Try[(EncryHistory, History.ProgressInfo[EncryPersistentModifier])] = {
    log.debug(s"Trying to append modifier ${Base58.encode(modifier.id)} of type ${modifier.modifierTypeId} to history...")
    testApplicable(modifier).map { _ =>
      modifier match {
        case header: EncryBlockHeader => (this, process(header))
        case payload: EncryBlockPayload => (this, process(payload))
      }
    }
  }

  private def testApplicable(modifier: EncryPersistentModifier): Try[Unit] = {
    modifier match {
      case header: EncryBlockHeader => validate(header)
      case payload: EncryBlockPayload => validate(payload)
      case mod: Any => Failure(new Error(s"Modifier $mod is of incorrect type."))
    }
  }

  // Checks whether the `modifier` is applicable to the `history`.
  override def applicable(modifier: EncryPersistentModifier): Boolean = testApplicable(modifier).isSuccess

  def lastHeaders(count: Int): EncryHeaderChain = bestHeaderOpt
    .map(bestHeader => headerChainBack(count, bestHeader, b => false)).getOrElse(EncryHeaderChain.empty)

  /**
    * Gets ErgoPersistentModifier of type T by it's id if it is in history
    */
  // TODO:
  def typedModifierById[T <: EncryPersistentModifier](id: ModifierId): Option[T] = modifierById(id) match {
    case Some(m: T@unchecked) if m.isInstanceOf[T] => Some(m)
    case _ => None
  }

  // TODO:
  override def syncInfo(answer: Boolean): EncrySyncInfo = if (isEmpty) {
    EncrySyncInfo(answer, Seq())
  } else {
    EncrySyncInfo(answer, lastHeaders(EncrySyncInfo.MaxBlockIds).headers.map(_.id))
  }
}

object EncryHistory
