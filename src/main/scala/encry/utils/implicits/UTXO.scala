package encry.utils.implicits

import cats.kernel.Monoid
import encry.storage.VersionalStorage.{StorageKey, StorageValue}
import encry.view.state.UtxoState.StateChange

object UTXO {

  def combineAll[A: Monoid](as: List[A]): A =
    as.foldLeft(Monoid[A].empty)(Monoid[A].combine)

  implicit def utxoStateChanges: Monoid[StateChange] = new Monoid[StateChange] {

    override def empty: StateChange = StateChange(Vector.empty[StorageKey], Vector.empty[(StorageKey, StorageValue)])

    override def combine(x: StateChange, y: StateChange): StateChange =
      StateChange(x.inputsToDb ++ y.inputsToDb, x.outputsToDb ++ y.outputsToDb)
  }

}
