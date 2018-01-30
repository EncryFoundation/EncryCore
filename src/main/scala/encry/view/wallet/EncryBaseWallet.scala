package encry.view.wallet

import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.PrivateKey25519

trait EncryBaseWallet extends WalletReader {

  def historyTransactions: Seq[WalletTransaction] = getAllTransactions.map(WalletTransaction)

  def boxes: Seq[WalletBox] = getAvailableBoxes.map(WalletBox)

  def publicKeys: Set[PublicKey25519Proposition]

  //todo: protection?
  def secrets: Set[PrivateKey25519]

  def secretByPublicImage(publicImage: PublicKey25519Proposition): Option[PrivateKey25519]
}
