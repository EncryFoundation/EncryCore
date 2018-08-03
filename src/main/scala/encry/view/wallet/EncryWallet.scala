package encry.view.wallet

import java.io.File

import com.google.common.primitives.Longs
import encry.crypto.PublicKey25519
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.mempool.BaseTransaction
import encry.modifiers.state.box.TokenIssuingBox.TokenId
import encry.modifiers.state.box.Box.Amount
import encry.modifiers.state.box.{EncryBaseBox, EncryProposition}
import encry.settings.{Algos, Constants, EncryAppSettings}
import encry.utils.{BalanceCalculator, BoxFilter, ByteStr, Logging}
import encry.view.wallet.keys.KeyManager
import encry.view.wallet.storage.WalletStorage
import encry.{ModifierId, VersionTag}
import io.iohk.iodb.{ByteArrayWrapper, LSMStore, Store}
import scorex.crypto.authds.ADKey

import scala.util.Try

case class EncryWallet(walletStore: Store, keyManager: KeyManager)
  extends Vault[EncryProposition, BaseTransaction, EncryPersistentModifier, EncryWallet] with Logging {

  val propositions: Set[EncryProposition] = publicKeys.map(pk => EncryProposition.pubKeyLocked(pk.pubKeyBytes))

  val walletStorage: WalletStorage = WalletStorage(walletStore, publicKeys)

  def publicKeys: Set[PublicKey25519] = keyManager.keys.foldLeft(Seq[PublicKey25519]()) {
    case (set, key) => set :+ PublicKey25519(key.publicKeyBytes)
  }.toSet

  override def scanOffchain(tx: BaseTransaction): EncryWallet = this

  override def scanOffchain(txs: Seq[BaseTransaction]): EncryWallet = this

  override def scanPersistent(modifier: EncryPersistentModifier): EncryWallet = modifier match {

    case block: EncryBlock =>
      val (newBxs: Seq[EncryBaseBox], spentBxs: Seq[EncryBaseBox]) = block.transactions.foldLeft(Seq[EncryBaseBox](), Seq[EncryBaseBox]()) {
        case ((nBxs, sBxs), tx: BaseTransaction) =>
          val newBxsL: Seq[EncryBaseBox] = tx.newBoxes
            .foldLeft(Seq[EncryBaseBox]()) { case (nBxs2, bx) =>
              if (propositions.exists(_.contractHash sameElements bx.proposition.contractHash)) nBxs2 :+ bx else nBxs2
            }
          val spendBxsIdsL: Seq[EncryBaseBox] =
            tx.inputs.filter(input => walletStorage.containsBox(input.boxId))
              .foldLeft(Seq.empty[EncryBaseBox]) { case (boxes, input) => walletStorage.getBoxById(input.boxId).map(bx => boxes :+ bx).getOrElse(boxes) }
          (nBxs ++ newBxsL) -> (sBxs ++ spendBxsIdsL)
      }

      updateWallet(modifier.id, newBxs, spentBxs)
      this

    case _ => this
  }

  override def rollback(to: VersionTag): Try[EncryWallet] = Try(walletStore.rollback(ByteArrayWrapper(to))).map(_ => this)

  private def calculateNewBalance(bxsToInsert: Seq[EncryBaseBox], bxsToRemove: Seq[EncryBaseBox]): Seq[(ByteArrayWrapper, ByteArrayWrapper)] = {
    import WalletStorage._
    val bObj: Map[TokenId, Amount] = BalanceCalculator.balanceSheet(bxsToInsert)
    val toRemove: Map[TokenId, Amount] = BalanceCalculator.balanceSheet(bxsToRemove)
    val prevBoxes: Map[TokenId, Amount] = getBalances.toMap
    val newBalanceSheet: Map[TokenId, Amount] = {
      (prevBoxes.toSeq ++ bObj.toSeq).map(elem => Algos.encode(elem._1) -> elem._2).groupBy(_._1).foldLeft(Map.empty[String, Amount]) {
        case (balanceMap, tokenInfo) => balanceMap.updated(tokenInfo._1, tokenInfo._2.foldLeft(0L)((tokenSum, token) => tokenSum + token._2))
      }.map(element => ADKey @@ Algos.decode(element._1).getOrElse(Array.emptyByteArray) -> element._2).map(tokenInfo => tokenInfo._1 ->
        (tokenInfo._2 - toRemove.find(ti => java.util.Arrays.equals(ti._1, tokenInfo._1)).map(_._2).getOrElse(0L)))
    }

    val decodedTokenBalance: Seq[(ByteArrayWrapper, ByteArrayWrapper)] = newBalanceSheet.foldLeft(Seq[(ByteArrayWrapper, ByteArrayWrapper)]()) {
        case (seq, (tId, balance)) => seq :+ (ByteArrayWrapper(tId) -> ByteArrayWrapper(Longs.toByteArray(balance)))
      }
    decodedTokenBalance ++ Seq(tokensIdsKey -> ByteArrayWrapper(bObj.keys.foldLeft(Array.empty[Byte]) {
      case (acc, k) => acc ++ k
    }))
  }

  private def updateWallet(modifierId: ModifierId, newBxs: Seq[EncryBaseBox], spentBxs: Seq[EncryBaseBox]): Unit = {
    import WalletStorage._
    val bxsToInsert: Seq[EncryBaseBox] = newBxs.filter(bx => !spentBxs.contains(bx))
    val newBalance: Seq[(ByteArrayWrapper, ByteArrayWrapper)] =
      calculateNewBalance(BoxFilter.filterAmountCarryingBxs(bxsToInsert), spentBxs.filter(bx => !newBxs.contains(bx)))
    val toRemoveSummary: Seq[ByteArrayWrapper] = spentBxs.map(bx => keyByBoxId(bx.id))
    val toInsertSummary: Seq[(ByteArrayWrapper, ByteArrayWrapper)] = newBalance ++
      BoxFilter.filterAmountCarryingBxs(bxsToInsert).map(bx => keyByBoxId(bx.id) -> ByteArrayWrapper(bx.bytes))
    walletStorage.update(ByteArrayWrapper(modifierId), toRemoveSummary, toInsertSummary)
  }

  def getBalances: Seq[(TokenId, Long)] = walletStorage.getTokensId.foldLeft(Seq[(TokenId, Long)]()) {
    case (seq, tokenId) => walletStorage.getTokenBalanceById(tokenId) match {
      case Some(v) => seq :+ (tokenId, v)
      case None => seq
    }
  }
}

object EncryWallet {

  def getWalletDir(settings: EncryAppSettings): File = new File(s"${settings.directory}/wallet")

  def readOrGenerate(settings: EncryAppSettings): EncryWallet = {
    val walletDir: File = getWalletDir(settings)
    walletDir.mkdirs()
    val walletStore: LSMStore = new LSMStore(walletDir, keepVersions = Constants.DefaultKeepVersions)
    EncryWallet(walletStore, keyManager = KeyManager.readOrGenerate(settings))
  }
}