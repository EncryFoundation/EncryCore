package encry.view.wallet

import java.io.File

import com.google.common.primitives.Longs
import encry.crypto.{PrivateKey25519, PublicKey25519}
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.mempool.EncryBaseTransaction
import encry.modifiers.state.box.EncryBaseBox
import encry.modifiers.state.box.proposition.{AccountProposition, EncryProposition, HeightProposition}
import encry.settings.{Constants, EncryAppSettings}
import encry.utils.BoxFilter
import encry.view.wallet.keys.KeyManager
import encry.view.wallet.storage.WalletStorage
import io.iohk.iodb.{ByteArrayWrapper, LSMStore, Store}
import scorex.core.transaction.wallet.Vault
import scorex.core.utils.ScorexLogging
import scorex.core.{ModifierId, VersionTag}
import scorex.crypto.authds.ADKey

import scala.util.Try

class EncryWallet(val walletStore: Store, val keyManager: KeyManager)
  extends EncryBaseWallet
  with Vault[EncryProposition, EncryBaseTransaction, EncryPersistentModifier, EncryWallet]
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
        val (newTxs, newBxs, newOpenBxs, spentBxsIds, spentOpenBxsIds) = block.transactions
          .foldLeft(Seq[EncryBaseTransaction](), Seq[EncryBaseBox](), Seq[EncryBaseBox](), Seq[ADKey](), Seq[ADKey]()) {
            case ((nTxs, nBxs, nOpenBxs, sBxs, sOpenBxs), tx) =>
              val (newBxsL, newOpenBxsL) = tx.newBoxes
                .foldLeft(Seq[EncryBaseBox](), Seq[EncryBaseBox]()) { case ((nBxs2, nOpenBxs2), bx) => bx.proposition match {
                  case ap: AccountProposition if publicKeys.exists(_.address == ap.account.address) => (nBxs2 :+ bx) -> nOpenBxs2
                  case _: HeightProposition => nBxs2 -> (nOpenBxs2 :+ bx)
                  case _ => nBxs2 -> nOpenBxs2
                }}
              val (spendBxsIdsL, spentOpenBxsIdsL) = tx.unlockers.map(_.boxId)
                .foldLeft(Seq[ADKey](), Seq[ADKey]()) { case ((sBxs2, sOpenBxs2), id) =>
                  val bxsIdsCurrent = walletStorage.boxIds
                  val openBxsIdsCurrent = walletStorage.openBoxIds
                  if (bxsIdsCurrent.exists(_.sameElements(id))) {
                    (sBxs2 :+ id) -> sOpenBxs2
                  } else if (openBxsIdsCurrent.exists(_.sameElements(id))) {
                    sBxs2 -> (sOpenBxs2 :+ id)
                  } else {
                    sBxs2 -> sOpenBxs2
                  }
                }
            if (newBxsL.nonEmpty || publicKeys.exists(_.pubKeyBytes.sameElements(tx.accountPubKey.pubKeyBytes))) {
              (nTxs :+ tx, nBxs ++ newBxsL, nOpenBxs ++ newOpenBxsL, sBxs ++ spendBxsIdsL, sOpenBxs ++ spentOpenBxsIdsL)
            } else {
              (nTxs, nBxs, nOpenBxs ++ newOpenBxsL, sBxs, sOpenBxs ++ spentOpenBxsIdsL)
            }
          }
        updateWallet(modifier.id, newTxs, newBxs, newOpenBxs, spentBxsIds, spentOpenBxsIds)
        this
      case _ =>
          this
    }
  }

  override def rollback(to: VersionTag): Try[EncryWallet] = {
    val wrappedVersion = ByteArrayWrapper(to)
    Try(walletStore.rollback(wrappedVersion)).map(_ => this)
  }

  private def updateWallet(modifierId: ModifierId,
                           newTxs: Seq[EncryBaseTransaction],
                           newBxs: Seq[EncryBaseBox],
                           newOpenBxs: Seq[EncryBaseBox],
                           spentBxsIds: Seq[ADKey],
                           spentOpenBxsIds: Seq[ADKey]): Unit = {

    import WalletStorage._

    val bxsIdsCurrent = walletStorage.boxIds
    val openBxsIdsCurrent = walletStorage.openBoxIds

    val txIdsToInsertRaw = ByteArrayWrapper(walletStorage.get(transactionIdsKey).getOrElse(Array[Byte]()) ++
      newTxs.map(_.id).foldLeft(Array[Byte]())(_ ++ _))

    val bxsToInsert = newBxs.filter(bx => !spentBxsIds.exists(_.sameElements(bx.id)))

    val openBxsToInsert = newOpenBxs.filter(bx => !spentOpenBxsIds.exists(_.sameElements(bx.id)))

    val bxIdsPacked =
      walletStorage.packBoxIds(bxsIdsCurrent.filter(id =>
        !spentBxsIds.exists(_ sameElements id)) ++ bxsToInsert.map(_.id))

    val openBxIdsPacked =
      walletStorage.packBoxIds(openBxsIdsCurrent.filter(id =>
        !spentOpenBxsIds.exists(_ sameElements id)) ++ openBxsToInsert.map(_.id))

    val newBalanceRaw = ByteArrayWrapper(Longs.toByteArray(BoxFilter.filterAmountCarryingBxs(availableBoxes.filter(bx =>
      !spentBxsIds.exists(_ sameElements bx.id))).foldLeft(0L)(_ + _.amount) +
        BoxFilter.filterAmountCarryingBxs(bxsToInsert).foldLeft(0L)(_ + _.amount)))

    val toRemoveSummary = (spentBxsIds ++ spentOpenBxsIds).map(keyByBoxId)
    val toInsertSummary =
      Seq(transactionIdsKey -> txIdsToInsertRaw, boxIdsKey -> bxIdsPacked,
        openBoxesIdsKey -> openBxIdsPacked, balanceKey -> newBalanceRaw) ++
        (bxsToInsert ++ openBxsToInsert).map(bx => keyByBoxId(bx.id) -> ByteArrayWrapper(bx.bytes)) ++
        newTxs.map(tx => txKeyById(tx.id) -> ByteArrayWrapper(tx.bytes))

    walletStorage.update(ByteArrayWrapper(modifierId), toRemoveSummary, toInsertSummary)
  }
}

object EncryWallet {

  def getWalletDir(settings: EncryAppSettings): File = new File(s"${settings.directory}/wallet")

  def readOrGenerate(settings: EncryAppSettings): EncryWallet = {

    val walletDir = getWalletDir(settings)
    walletDir.mkdirs()

    val walletStore = new LSMStore(walletDir, keepVersions = Constants.DefaultKeepVersions)

    new EncryWallet(walletStore, keyManager = KeyManager.readOrGenerate(settings))
  }
}
