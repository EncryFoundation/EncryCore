package encry.view.wallet

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.mempool.EncryBaseTransaction
import scorex.core.VersionTag
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.{Proposition, PublicKey25519Proposition}
import scorex.core.transaction.state.PrivateKey25519
import scorex.core.transaction.wallet.{Wallet, WalletBox, WalletTransaction}
import scorex.core.utils.ScorexLogging

import scala.util.{Success, Try}

class EncryWallet
  extends Wallet[Proposition, EncryBaseTransaction, EncryPersistentModifier, EncryWallet]
    with ScorexLogging {

  override type S = PrivateKey25519
  override type PI = PublicKey25519Proposition

  // TODO: Implement correctly. The following code is temporary.

  override def secretByPublicImage(publicImage: PublicKey25519Proposition): Option[Nothing] = None

  override def generateNewSecret(): EncryWallet = this

  override def historyTransactions: Seq[WalletTransaction[Proposition, EncryBaseTransaction]] = ???

  override def boxes(): Seq[WalletBox[Proposition, Box[Proposition]]] = ???

  override def publicKeys: Set[PI] = Set()

  override def secrets: Set[S] = Set()

  //todo: implement
  override def scanOffchain(tx: EncryBaseTransaction): EncryWallet = this

  //todo: implement
  override def scanOffchain(txs: Seq[EncryBaseTransaction]): EncryWallet = this

  //todo: implement
  override def scanPersistent(modifier: EncryPersistentModifier): EncryWallet = this

  //todo: implement
  override def rollback(to: VersionTag): Try[EncryWallet] = Success(this)

  override type NVCT = this.type
}
