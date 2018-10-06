package encry.view.wallet

import java.io.File
import com.google.common.primitives.Longs
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.Block
import encry.modifiers.mempool.Transaction
import encry.modifiers.state.box.Box.Amount
import encry.modifiers.state.box.TokenIssuingBox.TokenId
import encry.modifiers.state.box.{EncryBaseBox, EncryProposition, MonetaryBox}
import encry.settings.EncryAppSettings
import encry.utils.CoreTaggedTypes.VersionTag
import encry.utils.BalanceCalculator
import io.iohk.iodb.{ByteArrayWrapper, LSMStore, Store}
import org.encryfoundation.common.crypto.PublicKey25519
import akka.pattern.ask
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Try
import encry.EncryApp.system
import encry.view.wallet.WalletHolder.{GetCurrentBalanceRequest, UpdateCurrentState}
import akka.util.ByteString
import org.encryfoundation.common.utils.TaggedTypes.ADKey
import scala.concurrent.Future

case class EncryWallet(walletStore: Store, accountManager: AccountManager) {

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
            val newBxsL: Seq[EncryBaseBox] = tx.newBoxes.foldLeft(Seq[EncryBaseBox]()) {
              case (nBxs2, bx) =>
                if (propositions.exists(_.contractHash sameElements bx.proposition.contractHash)) nBxs2 :+ bx else nBxs2
            }
            val spendBxsIdsL: Seq[EncryBaseBox] = tx.inputs
              .filter(input => walletStorage.containsBox(input.boxId))
              .foldLeft(Seq.empty[EncryBaseBox]) { case (boxes, input) =>
                walletStorage.getBoxById(input.boxId).map(bx => boxes :+ bx).getOrElse(boxes)
              }
            (nBxs ++ newBxsL) -> (sBxs ++ spendBxsIdsL)
        }

      updateWallet(newBxs, spentBxs)
      this

    case _ => this
  }

  def rollback(to: VersionTag): Try[EncryWallet] = Try(walletStore.rollback(ByteArrayWrapper(to))).map(_ => this)

  private def calculateNewBalance1(bxsToInsert: Seq[EncryBaseBox], bxsToRemove: Seq[EncryBaseBox]): ByteString = {
    val balancesToInsert: Map[ByteString, Amount] = BalanceCalculator.balanceSheet(bxsToInsert)
      .map(elem => ByteString.fromArray(elem._1) -> elem._2)
    val balancesToRemove: Map[ByteString, Amount] = BalanceCalculator.balanceSheet(bxsToRemove)
      .map(elem => ByteString.fromArray(elem._1) -> elem._2)
    val oldBalances: Future[Map[ByteString, Amount]] =
      (system.actorSelection("/user/walletHolder") ? GetCurrentBalanceRequest()) (5.seconds)
        .mapTo[Map[ByteString, Amount]]
    val newBalances: Future[Map[ByteString, Amount]] = oldBalances.map { old =>
      (old.toSeq ++ balancesToInsert.toSeq).groupBy(_._1).foldLeft(Map[ByteString, Amount]()) { case (balanceMap, tokenInfo) =>
        balanceMap.updated(tokenInfo._1, tokenInfo._2.foldLeft(0L)((tokenSum, token) => tokenSum + token._2))
      }.map(tokenInfo => tokenInfo._1 -> (tokenInfo._2 - balancesToRemove.filter(_._1 == tokenInfo._1).values.sum))
    }

    newBalances.map(balances => balances.foldLeft(ByteString.empty) { case (acc, (id, balance)) =>
      acc ++ id ++ Longs.toByteArray(balance)
    }).value.map(x => x.getOrElse(ByteString.empty)).getOrElse(ByteString.empty)
  }

  def updateWallet(newBoxes: Seq[EncryBaseBox], spentBoxes: Seq[EncryBaseBox]): Unit = {
    val boxesToInsert: Seq[EncryBaseBox] = newBoxes.diff(spentBoxes)
    val newBalance: ByteString = calculateNewBalance1(
      filterAmountCarryingBxs(boxesToInsert),
      spentBoxes.diff(boxesToInsert)
    )
    val toRemoveSummary: Seq[ADKey] = spentBoxes.map(box => box.id)
    val toInsertSummary: Seq[(ADKey, ByteString)] =
      filterAmountCarryingBxs(boxesToInsert).map(box => box.id -> ByteString.fromArray(box.bytes))

    system.actorSelection("user/walletHolder") ! UpdateCurrentState(newBalance, toInsertSummary, toRemoveSummary)
  }

  def getBalances: Seq[(TokenId, Long)] = walletStorage.getBalances.toSeq

  def filterAmountCarryingBxs(bxs: Seq[EncryBaseBox]): Seq[MonetaryBox] =
    bxs.foldLeft(Seq[MonetaryBox]())((acc, bx) => bx match {
      case acbx: MonetaryBox => acc :+ acbx
      case _ => acc
    })
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