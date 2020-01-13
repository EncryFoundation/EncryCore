package encry.view.history

sealed trait ValidationError
object ValidationError {
  sealed trait FatalValidationError extends ValidationError
  object FatalValidationError {
    case class GenesisBlockFatalValidationError(error: String) extends FatalValidationError
    case class HeaderFatalValidationError(error: String)       extends FatalValidationError
    case class PayloadFatalValidationError(error: String)      extends FatalValidationError
    case class UnknownModifierFatalError(error: String)        extends FatalValidationError
    case class IncorrectProcessingRegime(error: String)        extends FatalValidationError
  }
  sealed trait NonFatalValidationError extends ValidationError
  object NonFatalValidationError {
    case class HeaderNonFatalValidationError(error: String)  extends NonFatalValidationError
    case class PayloadNonFatalValidationError(error: String) extends NonFatalValidationError
  }
  case class HistoryApiError(error: String) extends ValidationError
}
