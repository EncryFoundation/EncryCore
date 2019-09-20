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

  implicit def adKeyMonoid: Monoid[ADKey] = new Monoid[ADKey] {

    override def empty: ADKey = ADKey @@ Array.emptyByteArray

    override def combine(x: ADKey, y: ADKey): ADKey = ADKey @@ (x ++ y)
  }

  implicit def adKeyOrder: Order[ADKey] = new Order[ADKey] {
    override def compare(x: ADKey, y: ADKey): Int = Arrays.compareUnsigned(x, y)
  }

  implicit def adKeyHashable: Hashable[ADKey] = new Hashable[ADKey] {
    override def hash(value: ADKey): Array[Byte] = Algos.hash(value)
  }

  implicit def arrayMonoid: Monoid[Array[Byte]] = new Monoid[Array[Byte]] {

    override def empty: Array[Byte] = Array.emptyByteArray

    override def combine(x: Array[Byte], y: Array[Byte]): Array[Byte] = x ++ y
  }

  implicit def adKeyConvert: ConvertableToStorage[ADKey] = new ConvertableToStorage[ADKey] {
    override def convertToStorage(firstValue: StorageKey): ADKey = ADKey !@@ firstValue
  }

  implicit def arrayByteConvert: ConvertableToStorage[Array[Byte]] = new ConvertableToStorage[Array[Byte]] {
    override def convertToStorage(firstValue: StorageKey): Array[Byte] = firstValue
  }
}
