package encry.view.wallet

import java.io.File

import com.google.common.primitives.Longs
import encry.account.Account
import encry.crypto.PublicKey25519
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.mempool.EncryBaseTransaction
import encry.modifiers.state.box.EncryBaseBox
import encry.modifiers.state.box.proposition.EncryProposition
import encry.settings.{Algos, Constants, EncryAppSettings}
import encry.utils.{BalanceCalculator, BoxFilter, ScorexLogging}
import encry.view.wallet.keys.KeyManager
import encry.view.wallet.storage.WalletStorage
import io.iohk.iodb.{ByteArrayWrapper, LSMStore, Store}
import encry.modifiers.state.box.Box.Amount
import scorex.core.{ModifierId, VersionTag}
import scorex.crypto.authds.ADKey
import scorex.utils.Random

import scala.util.Try

case class EncryWallet(walletStore: Store, keyManager: KeyManager)
  extends Vault[EncryProposition, EncryBaseTransaction, EncryPersistentModifier, EncryWallet] with ScorexLogging {

  val propositions: Set[EncryProposition] = publicKeys.map(pk => EncryProposition.accountLock(Account(pk.pubKeyBytes)))

  val walletStorage: WalletStorage = WalletStorage(walletStore, publicKeys)

  def publicKeys: Set[PublicKey25519] = keyManager.keys.foldLeft(Seq[PublicKey25519]()) {
    case (set, key) => set :+ PublicKey25519(key.publicKeyBytes)
  }.toSet

  override def scanOffchain(tx: EncryBaseTransaction): EncryWallet = this

  override def scanOffchain(txs: Seq[EncryBaseTransaction]): EncryWallet = this

  override def scanPersistent(modifier: EncryPersistentModifier): EncryWallet = modifier match {

      case block: EncryBlock =>
        val (
          newTxs: Seq[EncryBaseTransaction],
          newBxs: Seq[EncryBaseBox],
          spentBxsIds: Seq[ADKey]) = block.transactions
          .foldLeft(Seq[EncryBaseTransaction](), Seq[EncryBaseBox](), Seq[ADKey]()) {
            case ((nTxs, nBxs, sBxs), tx: EncryBaseTransaction) =>
              val newBxsL: Seq[EncryBaseBox] = tx.newBoxes
                .foldLeft(Seq[EncryBaseBox]()) { case (nBxs2, bx) =>
                  if (propositions.exists(_.contractHash sameElements bx.proposition.contractHash)) nBxs2 :+ bx else nBxs2
                }
              val spendBxsIdsL: Seq[ADKey] = tx.inputs.map(_.boxId)
                .foldLeft(Seq[ADKey]()) { case (sBxs2, id) =>
                  val bxsIdsCurrent: Seq[ADKey] = walletStorage.boxIds
                  if (bxsIdsCurrent.exists(_.sameElements(id))) sBxs2 :+ id
                  else sBxs2
                }
              val isRelatedTransaction: Boolean = {
                val walletPropositionsSet: Set[ByteArrayWrapper] = propositions.map(p => ByteArrayWrapper(Algos.hash(p.bytes)))
                val txPropositionsSet: Set[ByteArrayWrapper] = tx.inputs.foldLeft(Seq.empty[ByteArrayWrapper]) { case (acc, input) =>
                  acc ++ input.proofs.foldLeft(Seq.empty[ByteArrayWrapper]) { case (acc2, proof) =>
                    acc2 :+ ByteArrayWrapper(Algos.hash(proof.bytes)) }
                }.toSet
                walletPropositionsSet.intersect(txPropositionsSet).nonEmpty ||
                  tx.defaultProofOpt.exists(pr => walletPropositionsSet.exists(_.data sameElements Algos.hash(pr.bytes)))
              }
              if (newBxsL.nonEmpty || isRelatedTransaction) (nTxs :+ tx, nBxs ++ newBxsL, sBxs ++ spendBxsIdsL)
              else (nTxs, nBxs, sBxs)
          }
        updateWallet(modifier.id, newTxs, newBxs, spentBxsIds)
        this

      case _ => this
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
                           spentBxsIds: Seq[ADKey]): Unit = {
    import WalletStorage._
    val bxsIdsCurrent: Seq[ADKey] = walletStorage.boxIds
    val txIdsToInsertRaw = ByteArrayWrapper(walletStorage.get(transactionIdsKey).getOrElse(Array[Byte]()) ++
      newTxs.map(_.id).foldLeft(Array[Byte]())(_ ++ _))
    val bxsToInsert: Seq[EncryBaseBox] = newBxs.filter(bx => !spentBxsIds.exists(_.sameElements(bx.id)))
    val bxIdsPacked: ByteArrayWrapper = walletStorage.packBoxIds(bxsIdsCurrent.filter(id =>
      !spentBxsIds.exists(_ sameElements id)) ++ bxsToInsert.map(_.id))
    val newBalance: Seq[(ByteArrayWrapper, ByteArrayWrapper)] =
      calculateNewBalance(BoxFilter.filterAmountCarryingBxs(walletStorage.allBoxes.filter(bx =>
      !spentBxsIds.exists(_ sameElements bx.id))) ++ BoxFilter.filterAmountCarryingBxs(bxsToInsert))
    val toRemoveSummary: Seq[ByteArrayWrapper] = spentBxsIds.map(keyByBoxId)
    val toInsertSummary: Seq[(ByteArrayWrapper, ByteArrayWrapper)] =
      Seq(transactionIdsKey -> txIdsToInsertRaw, boxIdsKey -> bxIdsPacked) ++ newBalance ++
        bxsToInsert.map(bx => keyByBoxId(bx.id) -> ByteArrayWrapper(bx.bytes)) ++
        newTxs.map(tx => txKeyById(tx.id) -> ByteArrayWrapper(tx.bytes))

    walletStorage.update(ByteArrayWrapper(modifierId), toRemoveSummary, toInsertSummary)
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