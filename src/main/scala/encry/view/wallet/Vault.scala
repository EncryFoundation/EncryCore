package encry.view.wallet

import encry.modifiers.mempool.Transaction
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.{NodeViewComponent, PersistentNodeViewModifier, VersionTag}

import scala.util.Try

trait Vault[P <: Proposition, TX <: Transaction[P],
            PMOD <: PersistentNodeViewModifier, V <: Vault[P, TX, PMOD, V]] extends NodeViewComponent {
  self: V =>

  def scanOffchain(tx: TX): V

  def scanOffchain(txs: Seq[TX]): V

  def scanPersistent(modifier: PMOD): V

  def scanPersistent(modifiers: Option[PMOD]): V = modifiers.foldLeft(this) { case (v, mod) =>
    v.scanPersistent(mod)
  }

  def rollback(to: VersionTag): Try[V]
}