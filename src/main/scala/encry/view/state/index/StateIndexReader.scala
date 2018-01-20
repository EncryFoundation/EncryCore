package encry.view.state.index

import com.google.common.primitives.Bytes
import encry.crypto.Address
import encry.modifiers.state.box.proposition.AddressProposition
import encry.settings.Algos
import encry.view.state.index.storage.StateIndexStorage
import io.iohk.iodb.{ByteArrayWrapper, Store}
import scorex.core.ModifierId
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.ADKey
import scorex.crypto.encode.Base58
import scorex.utils.Random

import scala.collection.mutable

trait StateIndexReader extends ScorexLogging {

  val indexStore: Store

  protected val indexVersion: ModifierId = ModifierId @@ Random.randomBytes()

  protected lazy val indexStorage = new StateIndexStorage(indexStore)

  def boxesIdsByAddress(addr: Address): Option[Seq[ADKey]] = indexStorage.boxesByAddress(addr)

  // Updates or creates index for key `address`.
  def updateIndexFor(address: Address, toRemove: Seq[ADKey], toInsert: Seq[ADKey]): Unit = {
    val addrBytes = AddressProposition.getAddrBytes(address)
    val bxsOpt = indexStorage.boxesByAddress(address)
    bxsOpt match {
      case Some(bxs) =>
        indexStorage.update(
          ModifierId @@ Algos.hash(addrBytes ++ toRemove.head ++ toInsert.head),
          Seq(ByteArrayWrapper(addrBytes)),
          Seq(ByteArrayWrapper(addrBytes) -> ByteArrayWrapper(
            (bxs.filterNot(toRemove.contains) ++ toInsert)
              .foldLeft(Array[Byte]()) { case (buff, id) => buff ++ id }
          ))
        )
      case None =>
        indexStorage.update(
          ModifierId @@ Algos.hash(addrBytes ++ toRemove.head ++ toInsert.head),
          Seq(),
          Seq(ByteArrayWrapper(addrBytes) ->
            ByteArrayWrapper(toInsert.foldLeft(Array[Byte]()) { case (buff, id) => buff ++ id }))
        )
    }
  }

  def updateIndexBulk(version: ModifierId,
                      opsMap: mutable.HashMap[Address, (mutable.Set[ADKey], mutable.Set[ADKey])]): Unit = {
    val opsFinal = opsMap
      .foldLeft(Seq[(Address, Seq[ADKey])](), Seq[(Address, Seq[ADKey])]()) { case ((bNew, bExs), (addr, (toRem, toIns))) =>
      val bxsOpt = indexStorage.boxesByAddress(addr)
      bxsOpt match {
        case Some(bxs) =>
          bNew -> (bExs :+ (addr, bxs.filterNot(toRem.contains) ++ toIns.toSeq))
        case None =>
          (bNew :+ (addr, toIns.toSeq)) -> bExs
      }
    }
    // First remove existing records assoc with particular address.
    indexStorage.update(
      ModifierId @@ Algos.hash(version),
      opsFinal._2.map(i => ByteArrayWrapper(AddressProposition.getAddrBytes(i._1))),
      Seq()
    )
    // Than insert new versions of records + new records.
    indexStorage.update(
      version,
      Seq(),
      (opsFinal._1 ++ opsFinal._2).map { case (addr, ids) =>
        ByteArrayWrapper(AddressProposition.getAddrBytes(addr)) ->
          ByteArrayWrapper(ids.foldLeft(Array[Byte]()) { case (buff, id) => buff ++ id })
      }
    )
  }
}
