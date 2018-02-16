package encry.view.state.index.storage

import encry.account.Address
import encry.view.EncryBaseStorage
import encry.view.state.index.StateIndexManager
import io.iohk.iodb.{ByteArrayWrapper, Store}
import scorex.core.ModifierId
import scorex.crypto.authds.ADKey


class StateIndexStorage(val db: Store) extends EncryBaseStorage {

  def boxIdsByAddress(addr: Address): Option[Seq[ADKey]] =
    parseComplexValue(StateIndexManager.keyByAddress(addr), 32).map(ADKey @@ _)

  def updateWithReplacement(id: ModifierId,
                            idsToReplace: Seq[ByteArrayWrapper],
                            toInsert: Seq[(ByteArrayWrapper, ByteArrayWrapper)]): Unit =
    updateWithReplacement(ByteArrayWrapper(id), idsToReplace, toInsert)
}
