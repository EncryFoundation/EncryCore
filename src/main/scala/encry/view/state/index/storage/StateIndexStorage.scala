package encry.view.state.index.storage

import encry.account.Address
import encry.view.EncryBaseStorage
import encry.view.state.index.StateIndexManager
import io.iohk.iodb.Store
import scorex.crypto.authds.ADKey

class StateIndexStorage(val db: Store) extends EncryBaseStorage {

  def boxIdsByAddress(addr: Address): Option[Seq[ADKey]] =
    getAndUnpackComplexValue(StateIndexManager.keyByAddress(addr), 32).map(ADKey @@ _)
}
