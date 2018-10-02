package encry.view.wallet

import java.io.File
import com.google.common.primitives.Longs
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.Block
import encry.modifiers.mempool.Transaction
import encry.modifiers.state.box.Box.Amount
import encry.modifiers.state.box.TokenIssuingBox.TokenId
import encry.modifiers.state.box.{EncryBaseBox, EncryProposition}
import encry.settings.EncryAppSettings
import encry.utils.CoreTaggedTypes.{ModifierId, VersionTag}
import encry.utils.{BalanceCalculator, BoxFilter}
import io.iohk.iodb.{ByteArrayWrapper, LSMStore, Store}
import org.encryfoundation.common.crypto.PublicKey25519
import scala.util.Try

case class EncryWallet(walletStore: Store, accountManager: AccountManager) {

  import WalletStorage._

  val walletStorage: WalletStorage = WalletStorage(walletStore, publicKeys)

  def publicKeys: Set[PublicKey25519] = accountManager.publicAccounts.toSet

  def propositions: Set[EncryProposition] = publicKeys.map(pk => EncryProposition.pubKeyLocked(pk.pubKeyBytes))

  def scanOffchain(tx: Transaction): EncryWallet = this

  def scanOffchain(txs: Seq[Transaction]): EncryWallet = this

  def scanPersistent(modifier: EncryPersistentModifier): EncryWallet = modifier match {
    case block: Block =>
      val (newBxs: Seq[EncryBaseBox], spentBxs: Seq[EncryBaseBox]) =
        block.transactions.foldLeft(Seq[EncryBaseBox](), Seq[EncryBaseBox]()) {
          case ((nBxs, sBxs), tx: Transaction) =>
            val newBxsL: Seq[EncryBaseBox] = tx.newBoxes
              .foldLeft(Seq[EncryBaseBox]()) { case (nBxs2, bx) =>
                if (propositions.exists(_.contractHash sameElements bx.proposition.contractHash)) nBxs2 :+ bx else nBxs2
              }
            val spendBxsIdsL: Seq[EncryBaseBox] = tx.inputs
              .filter(input => walletStorage.containsBox(input.boxId))
              .foldLeft(Seq.empty[EncryBaseBox]) { case (boxes, input) =>
                walletStorage.getBoxById(input.boxId).map(bx => boxes :+ bx).getOrElse(boxes)
              }
            (nBxs ++ newBxsL) -> (sBxs ++ spendBxsIdsL)
        }

      updateWallet(modifier.id, newBxs, spentBxs)
      this

    case _ => this
  }

  def rollback(to: VersionTag): Try[EncryWallet] = Try(walletStore.rollback(ByteArrayWrapper(to))).map(_ => this)

  private def calculateNewBalance(bxsToInsert: Seq[EncryBaseBox], bxsToRemove: Seq[EncryBaseBox]): ByteArrayWrapper = {
    val balancesToInsert: Map[ByteArrayWrapper, Amount] = BalanceCalculator.balanceSheet(bxsToInsert)
      .map(elt => ByteArrayWrapper(elt._1) -> elt._2)
    val balancesToRemove: Map[ByteArrayWrapper, Amount] = BalanceCalculator.balanceSheet(bxsToRemove)
      .map(elt => ByteArrayWrapper(elt._1) -> elt._2)
    val oldBalances: Map[ByteArrayWrapper, Amount] = getBalances
      .map(elt => ByteArrayWrapper(elt._1) -> elt._2)
      .toMap
    val newBalances: Map[ByteArrayWrapper, Amount] = (oldBalances.toSeq ++ balancesToInsert.toSeq)
      .groupBy(_._1)
      .foldLeft(Map.empty[ByteArrayWrapper, Amount]) { case (balanceMap, tokenInfo) =>
        balanceMap.updated(tokenInfo._1, tokenInfo._2.foldLeft(0L)((tokenSum, token) => tokenSum + token._2))
      }
      .map(tokenInfo => tokenInfo._1 -> (tokenInfo._2 - balancesToRemove.filter(_._1 == tokenInfo._1).values.sum))
    ByteArrayWrapper(
      newBalances.foldLeft(Array.empty[Byte]) { case (acc, (ByteArrayWrapper(id), balance)) =>
        acc ++ id ++ Longs.toByteArray(balance)
      }
    )
  }

  private def updateWallet(modifierId: ModifierId, newBxs: Seq[EncryBaseBox], spentBxs: Seq[EncryBaseBox]): Unit = {
    val bxsToInsert: Seq[EncryBaseBox] = newBxs.filter(bx => !spentBxs.contains(bx))
    val newBalances: ByteArrayWrapper = calculateNewBalance(
      BoxFilter.filterAmountCarryingBxs(bxsToInsert),
      spentBxs.filter(bx => !newBxs.contains(bx))
    )
    val toRemoveSummary: Seq[ByteArrayWrapper] = spentBxs.map(bx => keyByBoxId(bx.id))
    val toInsertSummary: Seq[(ByteArrayWrapper, ByteArrayWrapper)] = (balancesKey, newBalances) +:
      BoxFilter.filterAmountCarryingBxs(bxsToInsert).map(bx => keyByBoxId(bx.id) -> ByteArrayWrapper(bx.bytes))
    walletStorage.update(ByteArrayWrapper(modifierId), toRemoveSummary, toInsertSummary)
  }

  def getBalances: Seq[(TokenId, Long)] = walletStorage.getBalances.toSeq
}

object EncryWallet {

  def getWalletDir(settings: EncryAppSettings): File = new File(s"${settings.directory}/wallet")

  def getKeysDir(settings: EncryAppSettings): File = new File(s"${settings.directory}/keys")

  def readOrGenerate(settings: EncryAppSettings): EncryWallet = {
    val walletDir: File = getWalletDir(settings)
    walletDir.mkdirs()
    val keysDir: File = getKeysDir(settings)
    keysDir.mkdirs()
    val walletStore: LSMStore = new LSMStore(walletDir, keepVersions = 0)
    val accountManagerStore: LSMStore = new LSMStore(keysDir, keepVersions = 0, keySize = 33)
    EncryWallet(walletStore, AccountManager(accountManagerStore))
  }
}