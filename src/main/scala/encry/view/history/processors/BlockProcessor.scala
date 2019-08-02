package encry.view.history.processors

import com.typesafe.scalalogging.StrictLogging
import encry.consensus.History.ProgressInfo
import encry.modifiers.history.HeaderChain
import encry.view.history.processors.ValidationError.FatalValidationError._
import encry.view.history.processors.ValidationError.NonFatalValidationError._
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{Block, Header}
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
import org.encryfoundation.common.utils.constants.TestNetConstants
import org.encryfoundation.common.validation.ModifierSemanticValidity

import scala.util.Try
import cats.syntax.either._
import encry.view.history.HistoryExternalApi

trait BlockProcessor extends HistoryExternalApi with StrictLogging {

  import BlockProcessor._

  protected val auxHistory: Boolean = false

  /** Id of header that contains transactions and proofs */
  //todo change description
  //override def bestBlockIdOpt: Option[ModifierId] = historyStorage.get(BestBlockKey).map(ModifierId @@ _)

  //def getBlockByHeader(h: Header): Option[Block]

  //protected def isBlockDefined(h: Header): Boolean

  protected def commonBlockThenSuffixes(header1: Header, header2: Header): (HeaderChain, HeaderChain)

  protected[history] def continuationHeaderChains(header: Header, filterCond: Header => Boolean): Seq[Seq[Header]]

  //contains last n proccesed blocks




}

object BlockProcessor {

  type BlockProcessing = PartialFunction[ToProcess, ProgressInfo]

  case class ToProcess(fullBlock: Block,
                       newModRow: PersistentModifier,
                       newBestHeader: Header,
                       newBestChain: Seq[Block],
                       blocksToKeep: Int)
}