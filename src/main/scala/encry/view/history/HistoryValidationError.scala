package encry.view.history

sealed trait HistoryValidationError

object HistoryValidationError {
  sealed trait FatalValidationError extends HistoryValidationError
  object FatalValidationError {
    case class GenesisBlockFatalValidationError(error: String) extends FatalValidationError
    case class HeaderFatalValidationError(error: String)       extends FatalValidationError
    case class PayloadFatalValidationError(error: String)      extends FatalValidationError
    case class UnknownModifierFatalError(error: String)        extends FatalValidationError
    case class IncorrectProcessingRegime(error: String)        extends FatalValidationError
  }
  sealed trait NonFatalValidationError extends HistoryValidationError
  object NonFatalValidationError {
    case class HeaderNonFatalValidationError(error: String)  extends NonFatalValidationError
    case class PayloadNonFatalValidationError(error: String) extends NonFatalValidationError
  }
  final case class HistoryExtensionError(error: String) extends HistoryValidationError
}