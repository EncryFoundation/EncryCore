package encry.view.state.index

import akka.actor.ActorRef
import encry.crypto.Address
import encry.view.state.index.storage.StateIndexStorage
import io.iohk.iodb.Store
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.ADKey

trait StateIndexReader extends ScorexLogging {

  val indexUpdaterRef: Option[ActorRef]

  val indexStore: Store

  protected lazy val indexStorage = new StateIndexStorage(indexStore)

  def boxesIdsByAddress(addr: Address): Option[Seq[ADKey]] = indexStorage.boxesByAddress(addr)
}
