package encry.view.state.index

import encry.crypto.Address
import encry.modifiers.state.box.proposition.AddressProposition
import encry.view.state.index.storage.StateIndexStorage
import io.iohk.iodb.{ByteArrayWrapper, Store}
import scorex.core.ModifierId
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.ADKey
import scorex.utils.Random

trait StateIndexReader extends ScorexLogging {

  val indexStore: Store

  protected val indexVersion: ModifierId = ModifierId @@ Random.randomBytes()

  protected lazy val indexStorage = new StateIndexStorage(indexStore)

  def boxesIdsByAddress(addr: Address): Option[Seq[ADKey]] = indexStorage.boxesByAddress(addr)

  // Updates or creates index for key `address`.
  def updateIndexFor(address: Address, toRemove: Seq[ADKey], toInsert: Seq[ADKey]): Unit = {
    val addrBytes = AddressProposition.getAddrBytes(address)
    val bxsOpt = indexStorage.boxesByAddress(address)
    if (bxsOpt.isDefined) {
      indexStorage.update(
        ModifierId @@ Random.randomBytes(),
        Seq(ByteArrayWrapper(addrBytes)),
        Seq(ByteArrayWrapper(addrBytes) -> ByteArrayWrapper(
          (bxsOpt.get.filterNot(toRemove.contains) ++ toInsert)
            .foldLeft(Array[Byte]()) { case (buff, id) => buff ++ id }
        ))
      )
    } else {
      indexStorage.update(ModifierId @@ Random.randomBytes(), Seq(),
        Seq(ByteArrayWrapper(addrBytes) ->
          ByteArrayWrapper(toInsert.foldLeft(Array[Byte]()) { case (buff, id) => buff ++ id })))
    }
  }
}
