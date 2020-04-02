package encry.view.wallet

import org.encryfoundation.common.crypto.PublicKey25519

trait WalletReader {
  val accountManagers: Seq[AccountManager]
  def publicKeys: Set[PublicKey25519]
  def getBalances: Seq[((String, String), Long)]
}

object WalletReader {
  def apply(wallet: EncryWallet): WalletReader = new WalletReader {
    val accountManagers: Seq[AccountManager] = wallet.accountManagers

    override def publicKeys: Set[PublicKey25519] = wallet.publicKeys

    def getBalances: Seq[((String, String), Long)] = wallet.getBalances
  }
  def empty: WalletReader = new WalletReader {
    override val accountManagers: Seq[AccountManager] = Seq.empty

    override def publicKeys: Set[PublicKey25519] = Set.empty

    def getBalances: Seq[((String, String), Long)] = Seq.empty
  }
}
