package encry.view.history.tmp

import org.encryfoundation.common.modifiers.history.{ Block, Header }
import org.encryfoundation.common.utils.TaggedTypes.ModifierId

trait HistoryReader {

  def getHeaderById(id: ModifierId): Option[Header]

  def getBlockById(id: ModifierId): Option[Block]

  def getBestHeader: Option[Header]

  def getBestBlock: Option[Block]

}
