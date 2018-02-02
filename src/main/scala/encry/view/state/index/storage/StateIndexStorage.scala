package encry.view.state.index.storage

import encry.account.Address
import encry.modifiers.state.box.EncryBox
import encry.settings.Algos
import encry.view.EncryBaseStorage
import encry.view.state.index.StateIndexManager
import io.iohk.iodb.{ByteArrayWrapper, Store}
import scorex.core.ModifierId
import scorex.crypto.authds.ADKey

class StateIndexStorage(val db: Store) extends EncryBaseStorage {

  def boxIdsByAddress(addr: Address): Option[Seq[ADKey]] =
    db.get(ByteArrayWrapper(StateIndexManager.keyByAddress(addr))).flatMap { bytes =>
      Some((0 until (bytes.data.length / 32)).foldLeft(Seq[ADKey]()) { case (seq, i) =>
        seq :+ ADKey @@ bytes.data.slice(i * 32, i * 32 + 32)
      }).ensuring(_.forall(_.forall(i => i.length == EncryBox.BoxIdSize)))
    }

  def updateWithReplacement(id: ModifierId,
                            idsToRemove: Seq[ByteArrayWrapper],
                            toInsert: Seq[(ByteArrayWrapper, ByteArrayWrapper)]): Unit = {
    db.update(ByteArrayWrapper(Algos.hash(id)), idsToRemove, Seq())
    db.update(ByteArrayWrapper(id), Seq(), toInsert)
  }
}
