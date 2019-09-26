package encry.utils.implicits

import java.util

import cats.Order
import cats.kernel.Monoid
import encry.storage.VersionalStorage.{StorageKey, StorageValue}
import encry.view.state.UtxoState.StateChange
import encry.view.state.avlTree.utils.implicits.{ConvertableToStorage, Hashable}
import org.bouncycastle.util.Arrays
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ADKey

object UTXO {

  def combineAll[A: Monoid](as: List[A]): A =
    as.foldLeft(Monoid[A].empty)(Monoid[A].combine)

  implicit def utxoStateChanges: Monoid[StateChange] = new Monoid[StateChange] {

    override def empty: StateChange = StateChange(Vector.empty[StorageKey], Vector.empty[(StorageKey, StorageValue)])

    override def combine(x: StateChange, y: StateChange): StateChange =
      StateChange(x.inputsToDb ++ y.inputsToDb, x.outputsToDb ++ y.outputsToDb)
  }

//  implicit def arrayMonoid: Monoid[Array[Byte]] = new Monoid[Array[Byte]] {
//
//    override def empty: Array[Byte] = Array.emptyByteArray
//
//    override def combine(x: Array[Byte], y: Array[Byte]): Array[Byte] = x ++ y
//  }
}
