package encry.view.wallet

import java.io.File

import cats.implicits._
import com.typesafe.scalalogging.StrictLogging
import encry.settings.{EncryAppSettings, Settings}
import encry.storage.VersionalStorage
import encry.storage.VersionalStorage.{StorageKey, StorageValue}
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, WalletVersionalLevelDB, WalletVersionalLevelDBCompanion}
import encry.utils.CoreTaggedTypes.VersionTag
import io.iohk.iodb.{LSMStore, Store}
import encry.view.state.UtxoState
import encry.view.state.avlTree.{InternalNode, LeafNode, Node, ShadowNode}
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.encryfoundation.common.crypto.PublicKey25519
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.modifiers.state.StateModifierSerializer
import org.encryfoundation.common.modifiers.state.box.{EncryBaseBox, EncryProposition, MonetaryBox}
import org.encryfoundation.common.utils.TaggedTypes.{ADKey, ModifierId}
import org.iq80.leveldb.{DB, Options}
import scala.util.{Failure, Success, Try}

case class EncryWallet(walletStorage: WalletVersionalLevelDB, accountManagers: Seq[AccountManager], private val accountStore: Store)
  extends StrictLogging with AutoCloseable with Settings {

  assert(accountManagers.nonEmpty)

  def addAccount(seed: String, password: String): Option[EncryWallet] = if (accountManagers.map(_.number).max < Byte.MaxValue) {
    val newAccount = AccountManager(accountStore, password, seed.some, (accountManagers.map(_.number).max + 1).toByte)
    this.copy(accountManagers = accountManagers :+ newAccount).some
  } else {
    logger.warn("Maximum number of accounts exceeded")
    None
  }

  def publicKeys: Set[PublicKey25519] = accountManagers.flatMap(_.publicAccounts).toSet

  def propositions: Set[EncryProposition] = publicKeys.map(pk => EncryProposition.pubKeyLocked(pk.pubKeyBytes))

  def scanPersistent(modifier: PersistentModifier): EncryWallet = modifier match {
    case block: Block =>
      logger.info(s"Keys during sync: $publicKeys")
      val (newBxs: Seq[EncryBaseBox], spentBxs: Seq[EncryBaseBox]) =
        block.payload.txs.foldLeft(Seq[EncryBaseBox](), Seq[EncryBaseBox]()) {
          case ((nBxs, sBxs), tx: Transaction) =>
            val newBxsL: Seq[EncryBaseBox] = tx.newBoxes
              .foldLeft(Seq[EncryBaseBox]()) { case (nBxs2, bx) =>
                if (propositions.exists(_.contractHash sameElements bx.proposition.contractHash)) nBxs2 :+ bx else nBxs2
              }
            val spendBxsIdsL: Seq[EncryBaseBox] = tx.inputs
              .filter(input => walletStorage.containsBox(input.boxId))
              .foldLeft(Seq.empty[EncryBaseBox]) { case (boxes, input) =>
                walletStorage.getBoxById(input.boxId)
                  .map(bx => boxes :+ bx)
                  .getOrElse(boxes)
              }
            (nBxs ++ newBxsL) -> (sBxs ++ spendBxsIdsL)
        }
      walletStorage.updateWallet(modifier.id, newBxs, spentBxs, settings.constants.IntrinsicTokenId)
      this

    case _ => this
  }

  def scanWalletFromUtxo(state: UtxoState): EncryWallet = {
    val bxsToAdd = EncryWallet.scanTree(state.tree.rootNode, state.tree.storage, propositions)
    walletStorage.updateWallet(
      ModifierId !@@ state.tree.storage.currentVersion,
      bxsToAdd,
      List.empty,
      settings.constants.IntrinsicTokenId)
    this
  }

  def rollback(to: VersionTag): Try[Unit] = Try(walletStorage.rollback(ModifierId @@ to.untag(VersionTag)))

  def getBalances: Seq[(String, Long)] = walletStorage.getBalances.toSeq

  override def close(): Unit = walletStorage.close()
}

object EncryWallet extends StrictLogging {

  def getWalletDir(settings: EncryAppSettings): File = new File(s"${settings.directory}/wallet")

  def getKeysDir(settings: EncryAppSettings): File = new File(s"${settings.directory}/keys")

  def scanTree(node: Node[StorageKey, StorageValue],
               storage: VersionalStorage,
               accounts: Set[EncryProposition]): List[EncryBaseBox] = node match {
    case sh: ShadowNode[StorageKey, StorageValue] =>
      val restoredNode = sh.restoreFullNode(storage)
      scanTree(restoredNode, storage, accounts)
    case internalNode: InternalNode[StorageKey, StorageValue] =>
      StateModifierSerializer.parseBytes(internalNode.value, internalNode.key.head) match {
        case Success(bx) => collectBx(bx, accounts) :::
          internalNode.leftChild.map(leftNode => scanTree(leftNode, storage, accounts)).getOrElse(List.empty) :::
          internalNode.rightChild.map(rightChild => scanTree(rightChild, storage, accounts)).getOrElse(List.empty)
        case Failure(exception) => throw exception //???????
      }
    case leafNode: LeafNode[StorageKey, StorageValue] =>
      StateModifierSerializer.parseBytes(leafNode.value, leafNode.key.head) match {
        case Success(bx) => collectBx(bx, accounts)
        case Failure(exception) => throw exception //???????
      }
  }

  def collectBx(box: EncryBaseBox, accounts: Set[EncryProposition]): List[EncryBaseBox] = box match {
    case monetary: MonetaryBox if accounts.exists(_.contractHash sameElements monetary.proposition.contractHash) =>
      List(monetary)
    case _ => List.empty[MonetaryBox]
  }

  def readOrGenerate(settings: EncryAppSettings): EncryWallet = {
    val walletDir: File = getWalletDir(settings)
    walletDir.mkdirs()
    val keysDir: File = getKeysDir(settings)
    keysDir.mkdirs()
    val db: DB = LevelDbFactory.factory.open(walletDir, new Options)
    val accountManagerStore: LSMStore = new LSMStore(keysDir, keepVersions = 0, keySize = 34)
    val walletStorage = WalletVersionalLevelDBCompanion(db, settings.levelDB)
    val password: String = settings.wallet.map(_.password).getOrElse(throw new RuntimeException("Password not specified"))
    val restoredAccounts = AccountManager.restoreAccounts(accountManagerStore, password)
    val resultingAccounts =
      if (restoredAccounts.nonEmpty) restoredAccounts
      else Seq(AccountManager(accountManagerStore, password, settings.wallet.flatMap(_.seed), 0.toByte))
    //init keys
    EncryWallet(walletStorage, resultingAccounts, accountManagerStore)
  }
}