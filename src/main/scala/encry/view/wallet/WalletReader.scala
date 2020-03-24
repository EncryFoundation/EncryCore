package encry.view.wallet

trait WalletReader {
  val accountManagers: Seq[AccountManager]
}

object WalletReader {
  def apply(wallet: EncryWallet): WalletReader = new WalletReader {
    val accountManagers: Seq[AccountManager] = wallet.accountManagers
  }
  def empty: WalletReader = new WalletReader {
    override val accountManagers: Seq[AccountManager] = Seq.empty
  }
}
