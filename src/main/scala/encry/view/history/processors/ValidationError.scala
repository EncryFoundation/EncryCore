package encry.view.history.processors

sealed trait ValidationError

object ValidationError {
  sealed trait FatalValidationError extends ValidationError
  object FatalValidationError {
    case class IncorrectHeadersTimestamp(error: String) extends FatalValidationError
    case class IncorrectRealBlocksDifficulty(error: String) extends FatalValidationError
    case class IncorrectRequiredBlocksDifficulty(error: String) extends FatalValidationError
    case class IncorrectProofOfWorkSolution(error: String) extends FatalValidationError
    case class ModifiersHeightGreaterThanMaxRollBackDepth(error: String) extends FatalValidationError

    case class IncorrectGenesisBlockParentId(error: String) extends FatalValidationError
    case class GenesisBlockAppendedToNonEmptyHistory(error: String) extends FatalValidationError

    case class IncorrectModifiersHeight(error: String) extends FatalValidationError
    case class ExistedInHistoryModifier(error: String) extends FatalValidationError
    case class SemanticallyInvalidModifier(error: String) extends FatalValidationError
    case class NonRelatedModifier(error: String) extends FatalValidationError

    case class ModifierHasIncorrectType(error: String) extends FatalValidationError
    case class IncorrectProcessingRegime(error: String) extends FatalValidationError
  }
  sealed trait NonFatalValidationError extends ValidationError
  object NonFatalValidationError {
    case class TooYoungHeader(error: String) extends NonFatalValidationError
    case class TooOldModifier(error: String) extends NonFatalValidationError
    case class HistoryDoesNotContainModifiersParent(error: String) extends NonFatalValidationError
  }
}