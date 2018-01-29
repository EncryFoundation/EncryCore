package encry.view.wallet

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.mempool.{CoinbaseTransaction, EncryBaseTransaction, PaymentTransaction}
import encry.settings.EncryAppSettings
import encry.view.wallet.keys.KeyManager
import encry.view.wallet.storage.WalletStorageManager
import scorex.core.VersionTag
import scorex.core.transaction.box.proposition.{Proposition, PublicKey25519Proposition}
import scorex.core.transaction.state.PrivateKey25519
import scorex.core.transaction.wallet.Vault
import scorex.core.utils.{ByteStr, ScorexLogging}

import scala.util.{Success, Try}

case class EncryWallet(seed: ByteStr,
                       keyStorage: KeyManager)
  extends BaseWallet
  with Vault[Proposition, EncryBaseTransaction, EncryPersistentModifier, EncryWallet]
  with ScorexLogging {

  lazy val dataStorage: WalletStorageManager = WalletStorageManager.readOrGenerate(this)

  override def secretByPublicImage(publicImage: PublicKey25519Proposition): Option[PrivateKey25519] =
    keyStorage.keys.find(k => k.publicImage.address == publicImage.address)

  def historyTransactions: Seq[WalletTransaction] = dataStorage.getTrxs.map(WalletTransaction)

  def boxes: Seq[WalletBox] = dataStorage.getInstOfAllBoxes.map(WalletBox)

  override def secrets: Set[PrivateKey25519] = keyStorage.keys.toSet

  override def publicKeys: Set[PublicKey25519Proposition] = secrets.foldLeft(Seq[PublicKey25519Proposition]()){
    case(set, key) => set :+ PublicKey25519Proposition(key.publicKeyBytes)
  }.toSet

  override def scanOffchain(tx: EncryBaseTransaction): EncryWallet = this


  override def scanOffchain(txs: Seq[EncryBaseTransaction]): EncryWallet = this

  override def scanPersistent(modifier: EncryPersistentModifier): EncryWallet = {
    modifier match {
      case a: EncryBlock => a.transactions.foldLeft(this) { case (wallet, tx) =>
        tx match {
          case ptx: PaymentTransaction =>
            dataStorage.putTx(ptx)
            this
          case _: CoinbaseTransaction =>
            wallet
        }
      }
      case _: EncryBlockHeader => this
    }
  }

  def balance: Long = boxes.foldLeft(0L)(_ + _.box.amount)

  //todo: implement
  override def rollback(to: VersionTag): Try[EncryWallet] = Success(this)

  override type NVCT = this.type

}

object EncryWallet {

  def readOrGenerate(settings: EncryAppSettings): EncryWallet = {
    EncryWallet(
      ByteStr(settings.walletSettings.seed.getBytes()),
      keyStorage = KeyManager.readOrGenerate(settings)
    )
  }
}
