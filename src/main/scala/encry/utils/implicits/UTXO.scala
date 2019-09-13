package encry.utils.implicits

import cats.Foldable
import cats.kernel.Monoid
import encry.storage.VersionalStorage.{StorageKey, StorageValue}
import encry.view.state.UtxoState.StateChange
import scala.language.higherKinds

object UTXO {

  def combineAll[A: Monoid, K[_]](foldableWithMonoid: K[A])(implicit arg0: Foldable[K]): A =
    arg0.fold(foldableWithMonoid)

  implicit def utxoStateChanges: Monoid[StateChange] = new Monoid[StateChange] {

    override def empty: StateChange = StateChange(Vector.empty[StorageKey], Vector.empty[(StorageKey, StorageValue)])

    override def combine(x: StateChange, y: StateChange): StateChange =
      StateChange(x.inputsToDb ++ y.inputsToDb, x.outputsToDb ++ y.outputsToDb)
  }
}
