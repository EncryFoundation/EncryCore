package encry.view.wallet

import java.io.File

import com.google.common.primitives.Longs
import encry.account.Account
import encry.crypto.PublicKey25519
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.mempool.EncryBaseTransaction
import encry.modifiers.state.box.{CoinbaseBox, EncryBaseBox}
import encry.modifiers.state.box.proposition.{AccountProposition, EncryProposition, HeightProposition}
import encry.settings.{Algos, Constants, EncryAppSettings}
import encry.utils.{BalanceCalculator, BoxFilter}
import encry.view.history.Height
import encry.view.wallet.keys.KeyManager
import encry.view.wallet.storage.WalletStorage
import io.iohk.iodb.{ByteArrayWrapper, LSMStore, Store}
import scorex.core.transaction.box.Box.Amount
import scorex.core.transaction.wallet.Vault
import scorex.core.utils.ScorexLogging
import scorex.core.{ModifierId, VersionTag}
import scorex.crypto.authds.ADKey
import scorex.utils.Random

import scala.util.Try

case class EncryWallet(walletStore: Store, keyManager: KeyManager)
  extends Vault[EncryProposition, EncryBaseTransaction, EncryPersistentModifier, EncryWallet]
    with ScorexLogging {

  val propositions: Set[AccountProposition] = publicKeys.map(pk => AccountProposition(Account(pk.pubKeyBytes)))

  val walletStorage: WalletStorage = WalletStorage(walletStore, publicKeys)

  def publicKeys: Set[PublicKey25519] = keyManager.keys.foldLeft(Seq[PublicKey25519]()) {
    case (set, key) => set :+ PublicKey25519(key.publicKeyBytes)
  }.toSet

  override def scanOffchain(tx: EncryBaseTransaction): EncryWallet = this

  override def scanOffchain(txs: Seq[EncryBaseTransaction]): EncryWallet = this

  override def scanPersistent(modifier: EncryPersistentModifier): EncryWallet = {
    modifier match {
      case block: EncryBlock =>
        val (
          newTxs: Seq[EncryBaseTransaction],
          newBxs: Seq[EncryBaseBox],
          newOpenBxs: Seq[EncryBaseBox],
          spentBxsIds: Seq[ADKey],
          spentOpenBxsIds: Seq[ADKey]) = block.transactions
          .foldLeft(Seq[EncryBaseTransaction](), Seq[EncryBaseBox](), Seq[EncryBaseBox](), Seq[ADKey](), Seq[ADKey]()) {
            case ((nTxs, nBxs, nOpenBxs, sBxs, sOpenBxs), tx: EncryBaseTransaction) =>
              val (newBxsL: Seq[EncryBaseBox], newOpenBxsL: Seq[EncryBaseBox]) = tx.newBoxes
                .foldLeft(Seq[EncryBaseBox](), Seq[EncryBaseBox]()) {
                  case ((nBxs2, nOpenBxs2), bx) => bx.proposition match {
                    case ap: AccountProposition if publicKeys.exists(_.address == ap.account.address) => (nBxs2 :+ bx) -> nOpenBxs2
                    case _: HeightProposition if publicKeys.contains(block.header.accountPubKey) => nBxs2 -> (nOpenBxs2 :+ bx)
                    case _ => nBxs2 -> nOpenBxs2
                  }
                }
              val (spendBxsIdsL: Seq[ADKey], spentOpenBxsIdsL: Seq[ADKey]) = tx.unlockers.map(_.boxId)
                .foldLeft(Seq[ADKey](), Seq[ADKey]()) { case ((sBxs2, sOpenBxs2), id) =>
                  val bxsIdsCurrent: Seq[ADKey] = walletStorage.boxIds
                  val openBxsIdsCurrent: Seq[ADKey] = walletStorage.openBoxIds
                  if (bxsIdsCurrent.exists(_.sameElements(id))) (sBxs2 :+ id) -> sOpenBxs2
                  else if (openBxsIdsCurrent.exists(_.sameElements(id))) sBxs2 -> (sOpenBxs2 :+ id)
                  else sBxs2 -> sOpenBxs2
                }
              val isRelatedTransaction: Boolean = {
                val walletPropositionsSet: Set[ByteArrayWrapper] = propositions.map(p => ByteArrayWrapper(Algos.hash(p.bytes)))
                val txPropositionsSet: Set[ByteArrayWrapper] = tx.unlockers.foldLeft(Seq.empty[ByteArrayWrapper]) { case (acc, u) =>
                  u.proofOpt match {
                    case Some(proof) => acc :+ ByteArrayWrapper(Algos.hash(proof.bytes))
                    case _ => acc
                  }
                }.toSet
                walletPropositionsSet.intersect(txPropositionsSet).nonEmpty ||
                  tx.defaultProofOpt.exists(pr => walletPropositionsSet.exists(_.data sameElements Algos.hash(pr.bytes)))
              }
              if (newBxsL.nonEmpty || isRelatedTransaction)
                (nTxs :+ tx, nBxs ++ newBxsL, nOpenBxs ++ newOpenBxsL, sBxs ++ spendBxsIdsL, sOpenBxs ++ spentOpenBxsIdsL)
              else (nTxs, nBxs, nOpenBxs ++ newOpenBxsL, sBxs, sOpenBxs ++ spentOpenBxsIdsL)
          }
        updateWallet(modifier.id, newTxs, newBxs, newOpenBxs, spentBxsIds, spentOpenBxsIds)
        this
      case _ => this
    }
  }

  override def rollback(to: VersionTag): Try[EncryWallet] = Try(walletStore.rollback(ByteArrayWrapper(to))).map(_ => this)

  private def calculateNewBalance(bxs: Seq[EncryBaseBox]): Seq[(ByteArrayWrapper, ByteArrayWrapper)] = {
    import WalletStorage._
    val bObj: Map[ADKey, Amount] = BalanceCalculator.balanceSheet(bxs)
    val prevBoxes: Seq[(ADKey, Amount)] = getBalances
    val newBalanceSheet: Map[ADKey, Amount] = bObj.map(tokenInfo => tokenInfo._1 -> (tokenInfo._2 + prevBoxes
      .find(ti => ti._1 == tokenInfo._1).getOrElse((ADKey @@ Random.randomBytes(), 0L))._2))
    val decodedTokenBalance: Seq[(ByteArrayWrapper, ByteArrayWrapper)] =
      newBalanceSheet.foldLeft(Seq[(ByteArrayWrapper, ByteArrayWrapper)]()) {
        case (seq, (tId, balance)) => seq :+ (ByteArrayWrapper(tId) -> ByteArrayWrapper(Longs.toByteArray(balance)))
      }
    decodedTokenBalance ++ Seq(tokensIdsKey -> ByteArrayWrapper(bObj.keys.foldLeft(Array.empty[Byte]) {
      case (acc, k) => acc ++ k
    }))
  }

  private def updateWallet(modifierId: ModifierId,
                           newTxs: Seq[EncryBaseTransaction],
                           newBxs: Seq[EncryBaseBox],
                           newOpenBxs: Seq[EncryBaseBox],
                           spentBxsIds: Seq[ADKey],
                           spentOpenBxsIds: Seq[ADKey]): Unit = {
    import WalletStorage._
    val bxsIdsCurrent: Seq[ADKey] = walletStorage.boxIds
    val openBxsIdsCurrent: Seq[ADKey] = walletStorage.openBoxIds
    val txIdsToInsertRaw = ByteArrayWrapper(walletStorage.get(transactionIdsKey).getOrElse(Array[Byte]()) ++
      newTxs.map(_.id).foldLeft(Array[Byte]())(_ ++ _))
    val bxsToInsert: Seq[EncryBaseBox] = newBxs.filter(bx => !spentBxsIds.exists(_.sameElements(bx.id)))
    val openBxsToInsert: Seq[EncryBaseBox] = newOpenBxs.filter(bx => !spentOpenBxsIds.exists(_.sameElements(bx.id)))
    val bxIdsPacked: ByteArrayWrapper = walletStorage.packBoxIds(bxsIdsCurrent.filter(id =>
      !spentBxsIds.exists(_ sameElements id)) ++ bxsToInsert.map(_.id))
    val openBxIdsPacked: ByteArrayWrapper = walletStorage.packBoxIds(openBxsIdsCurrent.filter(id =>
      !spentOpenBxsIds.exists(_ sameElements id)) ++ openBxsToInsert.map(_.id))
    val newBalance: Seq[(ByteArrayWrapper, ByteArrayWrapper)] =
      calculateNewBalance(BoxFilter.filterAmountCarryingBxs(walletStorage.allBoxes.filter(bx =>
      !spentBxsIds.exists(_ sameElements bx.id))) ++ BoxFilter.filterAmountCarryingBxs(bxsToInsert))
    val toRemoveSummary: Seq[ByteArrayWrapper] = (spentBxsIds ++ spentOpenBxsIds).map(keyByBoxId)
    val toInsertSummary: Seq[(ByteArrayWrapper, ByteArrayWrapper)] =
      Seq(transactionIdsKey -> txIdsToInsertRaw, boxIdsKey -> bxIdsPacked,
        openBoxesIdsKey -> openBxIdsPacked) ++ newBalance ++
        (bxsToInsert ++ openBxsToInsert).map(bx => keyByBoxId(bx.id) -> ByteArrayWrapper(bx.bytes)) ++
        newTxs.map(tx => txKeyById(tx.id) -> ByteArrayWrapper(tx.bytes))

    walletStorage.update(ByteArrayWrapper(modifierId), toRemoveSummary, toInsertSummary)
  }

  def getAvailableCoinbaseBoxesAt(h: Height): Seq[CoinbaseBox] =
    walletStorage.openBoxIds.foldLeft(Seq[CoinbaseBox]()) { case (acc, id) =>
      walletStorage.getBoxById(id).map {
        case bx: CoinbaseBox if bx.proposition.height <= h => acc :+ bx
        case _ => acc
      }.getOrElse(acc)
    }

  def getBalances: Seq[(ADKey, Long)] = walletStorage.getTokensId.foldLeft(Seq[(ADKey, Long)]()) {
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