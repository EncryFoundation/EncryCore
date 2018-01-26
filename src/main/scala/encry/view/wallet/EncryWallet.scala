package encry.view.wallet

import encry.account.Address
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.mempool.{CoinbaseTransaction, EncryBaseTransaction, PaymentTransaction}
import encry.modifiers.state.box.EncryBaseBox
import encry.settings.EncryAppSettings
import encry.view.wallet.keys.KeyManager
import scorex.core.transaction.box.Box.Amount
import scorex.core.transaction.box.proposition.{Proposition, PublicKey25519Proposition}
import scorex.core.transaction.state.PrivateKey25519
import scorex.core.transaction.wallet.{Wallet, WalletBox, WalletTransaction}
import scorex.core.utils.{ByteStr, ScorexLogging}
import scorex.core.{ModifierId, VersionTag}
import scorex.crypto.signatures.Curve25519

import scala.util.{Success, Try}

case class EncryWallet(seed: ByteStr,
                       chainTransactions: Map[ModifierId, EncryBaseTransaction] = Map(),
                       offchainTransactions: Map[ModifierId, EncryBaseTransaction] = Map(),
                       currentBalance: Long = 0,
                       keyStorage: KeyManager)
  extends Wallet[Proposition, EncryBaseTransaction, EncryPersistentModifier, EncryWallet]
    with ScorexLogging {

  override type S = PrivateKey25519
  override type PI = PublicKey25519Proposition

  private val secret: S = {
    val pair = Curve25519.createKeyPair(seed.arr)
    PrivateKey25519(pair._1, pair._2)
  }

  override def secretByPublicImage(publicImage: PublicKey25519Proposition): Option[S] =
    keyStorage.keys.find(k => k.publicImage.address == publicImage.address)

  override def historyTransactions: Seq[WalletTransaction[Proposition, EncryBaseTransaction]] = ???

  override def boxes(): Seq[WalletBox[Proposition, EncryBaseBox]] = ???
//    chainTransactions
//          .filter(a => !(publicKeys contains a._2.proposition))
//          .foldLeft(Seq[(EncryBaseBox, EncryBaseTransaction)]()) {
//          case (seq, txCase) =>
//            seq ++ txCase._2.newBoxes
//              .filter(_.isInstanceOf[AssetBox])
//              .foldLeft(Seq[(EncryBaseBox, EncryBaseTransaction)]()) { case (buff, box) =>
//                buff ++ Seq((box, txCase._2))
//          }
//        }.map(i => WalletBox[PI, EncryBaseBox](i._1, i._2.id, i._2.timestamp)(AssetBoxSerializer))

  override def secrets: Set[PrivateKey25519] = keyStorage.keys.toSet

  override def publicKeys: Set[PublicKey25519Proposition] = secrets.foldLeft(Seq[PublicKey25519Proposition]()){
    case(set,key) => set :+ PublicKey25519Proposition(key.publicKeyBytes)
  }.toSet

  override def scanOffchain(tx: EncryBaseTransaction): EncryWallet = tx match {
    case sp: PaymentTransaction =>
      if ((sp.proposition.bytes sameElements secret.publicKeyBytes) || sp.createBoxes.foldRight(false) {
        (a: (Address, Amount), _: Boolean) => a._1 sameElements secret.publicKeyBytes }) {
        EncryWallet(seed, chainTransactions, offchainTransactions + (sp.id -> sp), currentBalance, keyStorage = this.keyStorage)
      } else this
    case ct: CoinbaseTransaction => this
  }

  override def scanOffchain(txs: Seq[EncryBaseTransaction]): EncryWallet = {
    txs.foldLeft(this) { case (wallet, tx) => wallet.scanOffchain(tx) }
  }

  override def scanPersistent(modifier: EncryPersistentModifier): EncryWallet = {
    modifier match {
      case a: EncryBlock => a.transactions.foldLeft(this) { case (w, tx) =>
        tx match {
          //TODO: not efficient
          case sp: PaymentTransaction =>
            if ((sp.proposition.bytes sameElements secret.publicKeyBytes) || sp.createBoxes.foldRight(false) {
              (a: (Address, Amount), _: Boolean) => a._1.getBytes() sameElements secret.publicKeyBytes
            }){
              val ct = w.chainTransactions + (sp.id -> sp)
              val oct = w.offchainTransactions - sp.id
              var curWalBal = w.currentBalance
              for (a <- sp.createBoxes) {
                if (sp.proposition.bytes sameElements secret.publicKeyBytes) {
                  curWalBal -= a._2
                } else if (a._1.getBytes sameElements secret.publicKeyBytes) {
                  curWalBal += a._2
                }
              }
              val cb = curWalBal
              EncryWallet(seed, ct, oct, cb,keyStorage = w.keyStorage)
            } else w
          case ct: CoinbaseTransaction => w
        }
      }
      case bh: EncryBlockHeader => this
    }
  }

  //todo: implement
  override def rollback(to: VersionTag): Try[EncryWallet] = Success(this)

  override type NVCT = this.type

  // Shadowed.
  override def generateNewSecret(): EncryWallet = this
}

object EncryWallet {

  def readOrGenerate(settings: EncryAppSettings): EncryWallet = {
    EncryWallet(
      ByteStr(settings.walletSettings.seed.getBytes()),
      Map(),
      Map(),
      keyStorage = KeyManager.readOrGenerate(settings)
    )
  }
}
