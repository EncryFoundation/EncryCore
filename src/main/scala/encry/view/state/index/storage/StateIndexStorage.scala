package encry.view.state.index.storage

import encry.account.Address
import encry.settings.Algos
import encry.view.EncryBaseStorage
import encry.view.state.index.StateIndexManager
import io.iohk.iodb.{ByteArrayWrapper, Store}
import scorex.core.ModifierId
import scorex.crypto.authds.ADKey

class StateIndexStorage(val db: Store) extends EncryBaseStorage {

  def boxIdsByAddress(addr: Address): Option[Seq[ADKey]] =
    getAndUnpackComplexValue(StateIndexManager.keyByAddress(addr), 32).map(ADKey @@ _)

  def updateWithReplacement(id: ModifierId,
                            idsToRemove: Seq[ByteArrayWrapper],
                            toInsert: Seq[(ByteArrayWrapper, ByteArrayWrapper)]): Unit = {
    db.update(ByteArrayWrapper(Algos.hash(id)), idsToRemove, Seq())
    db.update(ByteArrayWrapper(id), Seq(), toInsert)
  }
}
