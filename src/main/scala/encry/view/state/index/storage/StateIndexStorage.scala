package encry.view.state.index.storage

import encry.account.Address
import encry.modifiers.state.box.EncryBox
import encry.modifiers.state.box.proposition.AddressProposition
import io.iohk.iodb.{ByteArrayWrapper, Store}
import scorex.core.ModifierId
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.ADKey

class StateIndexStorage(val db: Store) extends ScorexLogging with AutoCloseable {

  def boxesByAddress(addr: Address): Option[Seq[ADKey]] =
    db.get(ByteArrayWrapper(AddressProposition.getAddrBytes(addr))).flatMap { bytes =>
      Some(bytes.data.sliding(EncryBox.BoxIdSize).map(ADKey @@ _).toSeq)
        .ensuring(_.forall(_.forall(i => i.length == EncryBox.BoxIdSize)))
    }

  def contains(id: ModifierId): Boolean = db.get(ByteArrayWrapper(id)).isDefined

  def insert(id: ModifierId, toInsert: Seq[(ByteArrayWrapper, ByteArrayWrapper)]): Unit = update(id, Seq(), toInsert)

  def update(id: ModifierId,
             idsToRemove: Seq[ByteArrayWrapper],
             toInsert: Seq[(ByteArrayWrapper, ByteArrayWrapper)]): Unit = {
    db.update(
      ByteArrayWrapper(id),
      idsToRemove,
      toInsert)
  }

  override def close(): Unit = {
    log.info("Closing state index storage...")
    db.close()
  }
}
