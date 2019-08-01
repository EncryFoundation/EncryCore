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






}