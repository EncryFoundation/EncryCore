package encry.view.wallet

import java.io.File

import com.google.common.primitives.Longs
import encry.crypto.{PrivateKey25519, PublicKey25519}
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.mempool.EncryBaseTransaction
import encry.modifiers.state.box.proposition.AccountProposition
import encry.modifiers.state.box.{AmountCarryingBox, EncryBaseBox}
import encry.settings.{Constants, EncryAppSettings}
import encry.view.wallet.keys.KeyManager
import encry.view.wallet.storage.WalletStorage
import io.iohk.iodb.{ByteArrayWrapper, LSMStore, Store}
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.wallet.Vault
import scorex.core.utils.ScorexLogging
import scorex.core.{ModifierId, VersionTag}
import scorex.crypto.authds.ADKey

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Try

class EncryWallet(val walletStore: Store, val keyManager: KeyManager)
  extends EncryBaseWallet
  with Vault[Proposition, EncryBaseTransaction, EncryPersistentModifier, EncryWallet]
  with ScorexLogging {

  override type NVCT = this.type

  override def secretByPublicImage(publicImage: PublicKey25519): Option[PrivateKey25519] =
    keyManager.keys.find(k => k.publicImage.address == publicImage.address)

  override def secrets: Set[PrivateKey25519] = keyManager.keys.toSet

  override def publicKeys: Set[PublicKey25519] = secrets.foldLeft(Seq[PublicKey25519]()){
    case (set, key) => set :+ PublicKey25519(key.publicKeyBytes)
  }.toSet

  val walletStorage: WalletStorage = new WalletStorage(walletStore, publicKeys)

  override def scanOffchain(tx: EncryBaseTransaction): EncryWallet = this

  override def scanOffchain(txs: Seq[EncryBaseTransaction]): EncryWallet = this

  override def scanPersistent(modifier: EncryPersistentModifier): EncryWallet = {
    modifier match {
      case block: EncryBlock =>
        val accountRelTxs = block.transactions.foldLeft(Seq[EncryBaseTransaction]())((acc, tx) => {
          val accountRelBxs = tx.newBoxes.foldLeft(Seq[EncryBaseBox]())((acc2, bx) => bx.proposition match {
            case ap: AccountProposition if publicKeys.exists(_.address == ap.account.address) => acc2 :+ bx
            case _ => acc2
          })
          if (accountRelBxs.nonEmpty || publicKeys.exists(_.pubKeyBytes.sameElements(tx.accountPubKey.pubKeyBytes))) acc :+ tx
          else acc
        })
        updateWallet(modifier.id, accountRelTxs)
        this
      case _ =>
          this
    }
  }

  override def rollback(to: VersionTag): Try[EncryWallet] = {
    val wrappedVersion = ByteArrayWrapper(to)
    Try(walletStore.rollback(wrappedVersion)).map(_ => this)
  }

  private def updateWallet(modifierId: ModifierId, newTxs: Seq[EncryBaseTransaction]): Future[Unit] = Future {

    import WalletStorage._

    def extractAcbxs(bxs: Seq[EncryBaseBox]): Seq[AmountCarryingBox] =
      bxs.foldLeft(Seq[AmountCarryingBox]())((acc, bx) => bx match {
        case acbx: AmountCarryingBox => acc :+ acbx
        case _ => acc
      })

    val currentBxIds = walletStorage.getBoxIds

    val txIdsToInsertRaw = ByteArrayWrapper(walletStorage.get(transactionIdsKey).getOrElse(Array[Byte]()) ++
      newTxs.map(_.id).foldLeft(Array[Byte]())(_ ++ _))
    val spentBxIds = newTxs.filter(tx => publicKeys.exists(_.pubKeyBytes sameElements tx.accountPubKey.pubKeyBytes))
      .flatMap(_.unlockers)
    val bxIdsToRemove = spentBxIds.foldLeft(Seq[ADKey]())((acc, u) =>
      if (currentBxIds.exists(_ sameElements u.boxId)) acc :+ u.boxId else acc)
    val bxsToInsert = newTxs.flatMap(_.newBoxes).foldLeft(Seq[EncryBaseBox]())((acc, bx) => bx.proposition match {
      case ap: AccountProposition
        if publicKeys.exists(_.address == ap.account.address) && !spentBxIds.exists(_.boxId sameElements bx.id) => acc :+ bx
      case _ => acc
    })

    val bxIdsToInsertRaw =
      walletStorage.packBoxIds(currentBxIds.filter(id =>
        !bxIdsToRemove.exists(_ sameElements id)) ++ bxsToInsert.map(_.id))

    val newBalanceRaw = ByteArrayWrapper(Longs.toByteArray(extractAcbxs(getAvailableBoxes.filter(bx =>
      !bxIdsToRemove.exists(_ sameElements bx.id))).foldLeft(0L)(_ + _.amount) +
      extractAcbxs(bxsToInsert).foldLeft(0L)(_ + _.amount)))

    val toRemoveSummary = bxIdsToRemove.map(boxKeyById) ++ Seq(balanceKey, transactionIdsKey, boxIdsKey)
    val toInsertSummary =
      Seq(transactionIdsKey -> txIdsToInsertRaw, boxIdsKey -> bxIdsToInsertRaw, balanceKey -> newBalanceRaw) ++
        bxsToInsert.map(bx => boxKeyById(bx.id) -> ByteArrayWrapper(bx.bytes)) ++
        newTxs.map(tx => txKeyById(tx.id) -> ByteArrayWrapper(tx.bytes))

    walletStorage.updateWithReplacement(modifierId, toRemoveSummary, toInsertSummary)
  }
}

object EncryWallet {

  def getWalletDir(settings: EncryAppSettings): File = new File(s"${settings.directory}/wallet")

  def readOrGenerate(settings: EncryAppSettings): EncryWallet = {

    val walletDir = getWalletDir(settings)
    walletDir.mkdirs()

    val walletStore = new LSMStore(walletDir, keepVersions = Constants.keepVersions)

    new EncryWallet(walletStore, keyManager = KeyManager.readOrGenerate(settings))
  }
}
