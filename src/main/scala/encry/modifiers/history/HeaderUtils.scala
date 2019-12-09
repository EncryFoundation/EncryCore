package encry.modifiers.history

import encry.settings.EncryAppSettings
import encry.view.history.History
import org.encryfoundation.common.modifiers.history.Header
import org.encryfoundation.common.validation.{ ModifierValidator, ValidationResult }

object HeaderUtils {

  val TransactionsRootSize: Int = 32

  def syntacticallyValidity(header: Header, modifierIdSize: Int): ValidationResult =
    ModifierValidator.accumulateErrors
      .demand(header.modifierTypeId == Header.modifierTypeId, s"Modifier's type id should be ${Header.modifierTypeId}")
      .demand(header.id.size == modifierIdSize, s"Modifier's id should be $modifierIdSize bytes")
      .demand(header.parentId.size == modifierIdSize, s"Parent's id should be $modifierIdSize bytes")
      .demand(header.transactionsRoot.size == TransactionsRootSize,
              s"TransactionsRoot's size should be $TransactionsRootSize bytes")
      .result

  def preSemanticValidation(header: Header, history: History, settings: EncryAppSettings): Either[PreSemanticValidationException, Unit] =
    for {
      _ <- Either.cond(
            history.getBestHeaderHeight - settings.constants.MaxRollbackDepth <= header.height,
            (),
            IllegalHeight(
              s"Height of received header is ${header.height}. " +
                s"Current best header height is ${history.getBestHeaderHeight}. " +
                s"Max possible height is ${history.getBestHeaderHeight - settings.constants.MaxRollbackDepth}"
            )
          )
    } yield ()

  sealed trait PreSemanticValidationException { val error: String }
  final case class IllegalHeight(error: String) extends PreSemanticValidationException
}
