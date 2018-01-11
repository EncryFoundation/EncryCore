package encry.view.wallet

import encry.crypto.Address
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.mempool.{EncryBaseTransaction, PaymentTransaction}
import encry.modifiers.state.box.EncryBaseBox
import encry.settings.EncryAppSettings
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
                       currentBalance: Long = 0
                       /*walletStore : LSMStore = new LSMStore(new File("wallet.store"))*/)
  extends Wallet[Proposition, EncryBaseTransaction, EncryPersistentModifier, EncryWallet]
    with ScorexLogging {

  override type S = PrivateKey25519
  override type PI = PublicKey25519Proposition

  //TODO: for wallet app should generate keys from file?
  private val secret: S = {
    val pair = Curve25519.createKeyPair(seed.arr)
    PrivateKey25519(pair._1, pair._2)
  }

  override def secretByPublicImage(publicImage: PublicKey25519Proposition): Option[S] =
    if (publicImage.address == secret.publicImage.address) Some(secret) else None

  //TODO: for Wallet app needs more than one secret

  override def generateNewSecret(): EncryWallet = throw new Error("Only one secret is supported")

  //TODO: need baseEncryPropos

  override def historyTransactions: Seq[WalletTransaction[Proposition, EncryBaseTransaction]] = Seq()

  override def boxes():  Seq[WalletBox[Proposition, EncryBaseBox]] = Seq()

  override def publicKeys: Set[PI] = Set(secret.publicImage)

  override def secrets: Set[S] = Set(secret)

  override def scanOffchain(tx: EncryBaseTransaction): EncryWallet = tx match {
    case sp: PaymentTransaction =>
      if ((sp.proposition.bytes sameElements secret.publicKeyBytes) || sp.createBoxes.foldRight(false) {
        (a: (Address, Amount), _: Boolean) => a._1 sameElements secret.publicKeyBytes }) {
        EncryWallet(seed, chainTransactions, offchainTransactions + (sp.id -> sp), currentBalance)
      } else this
  }

  override def scanOffchain(txs: Seq[EncryBaseTransaction]): EncryWallet = {
    txs.foldLeft(this) { case (wallet, tx) => wallet.scanOffchain(tx) }
  }

  override def scanPersistent(modifier: EncryPersistentModifier): EncryWallet =
  {
    modifier match {
      case a : EncryBlockPayload => a.transactions.foldLeft(this) { case (w, tx) =>
        tx match {
          //TODO: not efficient
          case sp: PaymentTransaction =>
            if ((sp.proposition.bytes sameElements secret.publicKeyBytes) || sp.createBoxes.foldRight(false) {
              (a: (Address, Amount), b: Boolean) => a._1.getBytes() sameElements secret.publicKeyBytes
            }){
              val ct = w.chainTransactions + (sp.id -> sp)
              val oct = w.offchainTransactions - sp.id
              var curWalBal = w.currentBalance
              var wB = for(a <- sp.createBoxes){
                if(sp.proposition.bytes sameElements secret.publicKeyBytes){
                  curWalBal-=a._2
                }else if(a._1.getBytes() sameElements secret.publicKeyBytes){
                  curWalBal += a._2
                }
              }
              val cb = curWalBal
              EncryWallet(seed, ct, oct, cb)
            } else w
        }
      }
    }
  }

  //todo: implement
  override def rollback(to: VersionTag): Try[EncryWallet] = Success(this)

  override type NVCT = this.type
}

object EncryWallet {
  def readOrGenerate(settings: EncryAppSettings): EncryWallet = {
    EncryWallet(ByteStr(settings.walletSettings.seed.getBytes()),Map(),Map(),0)
  }
}