package encry.view.history

sealed trait HistoryValidationResult
object HistoryValidationResult {
  case class HistoryDifficultyError(err: String) extends HistoryValidationResult
}