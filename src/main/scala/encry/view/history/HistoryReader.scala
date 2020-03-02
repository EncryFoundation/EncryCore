package encry.view.history

trait HistoryReader {

  def getBestHeaderHeight: Int

}

object HistoryReader {
  def empty: HistoryReader = new HistoryReader {
    def getBestHeaderHeight = 0
  }

  def apply(): HistoryReader = new HistoryReader {
    def getBestHeaderHeight = 1
  }
}