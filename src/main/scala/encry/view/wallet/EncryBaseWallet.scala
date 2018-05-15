package encry.view.wallet

import encry.crypto.{PrivateKey25519, PublicKey25519}


trait EncryBaseWallet extends WalletReader {

  def historyTransactions: Seq[WalletTransaction] = allTransactions.map(WalletTransaction)

  def boxes: Seq[WalletBox] = availableBoxes.map(WalletBox)

  def publicKeys: Set[PublicKey25519]

  def secrets: Set[PrivateKey25519]

  def secretByPublicImage(publicImage: PublicKey25519): Option[PrivateKey25519]
}
