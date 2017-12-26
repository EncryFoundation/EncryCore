package encry.view.wallet

import encry.modifiers.mempool.EncryBaseTransaction
import scorex.core.{NodeViewComponent, PersistentNodeViewModifier, VersionTag}
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.Proposition

import scala.util.Try


/**
  * Abstract interface for Vault, a storage for node-specific information
  */
trait EncryVault[PMOD <: PersistentNodeViewModifier, V <: EncryVault[PMOD, V]] extends NodeViewComponent {
  self: V =>

  def scanOffchain(tx: EncryBaseTransaction): V

  def scanOffchain(txs: Seq[EncryBaseTransaction]): V

  def scanPersistent(modifier: PMOD): V

  def scanPersistent(modifiers: Option[PMOD]): V = modifiers.foldLeft(this) { case (v, mod) =>
    v.scanPersistent(mod)
  }

  def rollback(to: VersionTag): Try[V]
}
