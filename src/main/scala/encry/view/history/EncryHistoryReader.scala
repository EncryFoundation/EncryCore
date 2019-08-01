package encry.view.history

import com.typesafe.scalalogging.StrictLogging
import encry.consensus.History._
import encry.modifiers.history._
import encry.settings.NodeSettings
import encry.view.history.processors.ValidationError.FatalValidationError.UnknownModifierFatalError
import encry.view.history.processors.ValidationError
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

trait EncryHistoryReader extends HistoryExternalApi
  with BlockPayloadProcessor
  with StrictLogging {

  protected val nodeSettings: NodeSettings

  /** Is there's no history, even genesis block */
  def isEmpty: Boolean = getBestHeaderId.isEmpty

  def contains(id: ModifierId): Boolean = historyStorage.containsMod(id)


  /** @return all possible forks, that contains specified header */
  protected[history] def continuationHeaderChains(header: Header,
                                                  filterCond: Header => Boolean): Seq[Seq[Header]] = {
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



  override def isSemanticallyValid(modifierId: ModifierId): ModifierSemanticValidity =
    historyStorage.store.get(validityKey(modifierId)) match {
      case Some(mod) if mod.headOption.contains(1.toByte) => ModifierSemanticValidity.Valid
      case Some(mod) if mod.headOption.contains(0.toByte) => ModifierSemanticValidity.Invalid
      case None if isModifierDefined(modifierId) => ModifierSemanticValidity.Unknown
      case None => ModifierSemanticValidity.Absent
      case mod => logger.error(s"Incorrect validity status: $mod")
        ModifierSemanticValidity.Absent
    }
}