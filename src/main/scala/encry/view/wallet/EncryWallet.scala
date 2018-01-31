package encry.view.wallet

import java.io.File

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.mempool.{CoinbaseTransaction, EncryBaseTransaction, PaymentTransaction}
import encry.settings.EncryAppSettings
import encry.view.wallet.keys.KeyManager
import encry.view.wallet.storage.WalletStorage
import io.iohk.iodb.{LSMStore, Store}
import scorex.core.VersionTag
import scorex.core.transaction.box.proposition.{Proposition, PublicKey25519Proposition}
import scorex.core.transaction.state.PrivateKey25519
import scorex.core.transaction.wallet.Vault
import scorex.core.utils.ScorexLogging

import scala.util.{Success, Try}

class EncryWallet(val walletStore: Store, val keyManager: KeyManager)
  extends EncryBaseWallet
  with Vault[Proposition, EncryBaseTransaction, EncryPersistentModifier, EncryWallet]
  with ScorexLogging {

  override def secretByPublicImage(publicImage: PublicKey25519Proposition): Option[PrivateKey25519] =
    keyManager.keys.find(k => k.publicImage.address == publicImage.address)

  override def secrets: Set[PrivateKey25519] = keyManager.keys.toSet

  override def publicKeys: Set[PublicKey25519Proposition] = secrets.foldLeft(Seq[PublicKey25519Proposition]()){
    case (set, key) => set :+ PublicKey25519Proposition(key.publicKeyBytes)
  }.toSet

  val walletStorage: WalletStorage = new WalletStorage(walletStore, publicKeys)

  override def scanOffchain(tx: EncryBaseTransaction): EncryWallet = this

  override def scanOffchain(txs: Seq[EncryBaseTransaction]): EncryWallet = this

  override def scanPersistent(modifier: EncryPersistentModifier): EncryWallet = {
    modifier match {
      case a: EncryBlock => a.transactions.foldLeft(this) { case (wallet, tx) =>
        tx match {
          case tx@(_: PaymentTransaction | _: CoinbaseTransaction) =>
            walletStorage.putTransaction(tx)
            this
          case _ => this // Do nothing.
        }
      }
      case _: EncryBlockHeader => this
    }
  }

  //todo: implement
  override def rollback(to: VersionTag): Try[EncryWallet] = Success(this)

  override type NVCT = this.type

}

object EncryWallet {

  def getWalletDir(settings: EncryAppSettings) = new File(s"${settings.directory}/wallet")

  def readOrGenerate(settings: EncryAppSettings): EncryWallet = {

    val walletDir = getWalletDir(settings)
    walletDir.mkdirs()

    val walletStore = new LSMStore(walletDir, keepVersions = 100)  // TODO: Move to constants.

    new EncryWallet(walletStore, keyManager = KeyManager.readOrGenerate(settings))
  }
}
