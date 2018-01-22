package encry.view.state.index

import akka.actor.ActorRef
import encry.account.Address
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

object StateIndexReader {

  val openBoxAddress: Address = Address @@ "3goCpFrrBakKJwxk7d4oY5HN54dYMQZbmVWKvQBPZPDvbL3hHp"
}
