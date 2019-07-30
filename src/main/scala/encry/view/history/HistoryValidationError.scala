package encry.view.history

sealed trait HistoryValidationError
object HistoryValidationError {
  sealed trait FatalValidationError extends HistoryValidationError
  object FatalValidationError {
    final case class GenesisBlockFatalValidationError(error: String) extends FatalValidationError
    final case class HeaderFatalValidationError(error: String)       extends FatalValidationError
    final case class PayloadFatalValidationError(error: String)      extends FatalValidationError
    final case class UnknownModifierFatalError(error: String)        extends FatalValidationError
    final case class IncorrectProcessingRegime(error: String)        extends FatalValidationError
  }
  sealed trait NonFatalValidationError extends HistoryValidationError
  object NonFatalValidationError {
    final case class HeaderNonFatalValidationError(error: String)  extends NonFatalValidationError
    final case class PayloadNonFatalValidationError(error: String) extends NonFatalValidationError
  }
  final case class HistoryExtensionError(error: String) extends HistoryValidationError
}