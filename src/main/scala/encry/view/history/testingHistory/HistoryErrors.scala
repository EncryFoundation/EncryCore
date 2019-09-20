package encry.view.history.testingHistory


object HistoryErrors {

  sealed trait HistoryError
  case class HistoryProcessingError(error: String) extends HistoryError
}