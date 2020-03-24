package encry.view.wallet

import java.io.File
import cats.data.NonEmptyChain._
import cats.data.{NonEmptyChain, Validated}
import cats.instances.string._
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.validated._
import com.typesafe.scalalogging.StrictLogging
import encry.settings.{EncryAppSettings, Settings}
import encry.storage.VersionalStorage
import encry.storage.VersionalStorage.{StorageKey, StorageValue}
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, WalletVersionalLevelDB, WalletVersionalLevelDBCompanion}
import encry.utils.CoreTaggedTypes.VersionTag
import encry.utils.Mnemonic
import encry.view.state.UtxoStateReader
import encry.view.state.avlTree.{InternalNode, LeafNode, Node, ShadowNode}
import io.iohk.iodb.{LSMStore, Store}
import org.encryfoundation.common.crypto.PublicKey25519
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.modifiers.state.StateModifierSerializer
import org.encryfoundation.common.modifiers.state.box.{EncryBaseBox, EncryProposition, MonetaryBox}
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
import org.iq80.leveldb.{DB, Options}
import scala.util.{Failure, Success, Try}

case class EncryWallet(walletStorage: WalletVersionalLevelDB, accountManagers: Seq[AccountManager], private val accountStore: Store)
  extends StrictLogging with AutoCloseable with Settings {

  assert(accountManagers.nonEmpty)

  def addAccount(seed: String, password: String, state: UtxoStateReader): Either[String, EncryWallet] = validateMnemonicKey(seed) match {
    case Right(_) if accountManagers.map(_.number).max < Byte.MaxValue =>
      val newAccount = AccountManager(accountStore, password, seed, (accountManagers.map(_.number).max + 1).toByte)
      val newAccPropositions = newAccount.publicAccounts.map(pk => EncryProposition.pubKeyLocked(pk.pubKeyBytes)).toSet
      scanWalletFromUtxo(state, newAccPropositions)
      this.copy(accountManagers = accountManagers :+ newAccount).asRight[String]
    case Right(_) => "Maximum number of accounts exceeded".asLeft[EncryWallet]
    case Left(reasons) => s"Invalid mnemonic, problems are: ${reasons.mkString_(", ")}".asLeft[EncryWallet]
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

  def scanWalletFromUtxo(state: UtxoStateReader, props: Set[EncryProposition]): EncryWallet = {
    val bxsToAdd: Seq[EncryBaseBox] = EncryWallet.scanTree(state.rootNode, state.avlStorage, props)
    if (bxsToAdd.nonEmpty)
      walletStorage.updateWallet(
        ModifierId !@@ state.avlStorage.currentVersion,
        bxsToAdd,
        List.empty,
        settings.constants.IntrinsicTokenId
      )
    this
  }

  def rollback(to: VersionTag): Try[Unit] = Try(walletStorage.rollback(ModifierId @@ to.untag(VersionTag)))

  def getBalances: Seq[((String, String), Long)] = {
    val pubKeys = publicKeys
    val contractHashToKey = contractHashesToKeys(pubKeys)
    val positiveBalance = walletStorage.getBalances.map { case ((hash, tokenId), amount) =>
      (contractHashToKey(hash), tokenId) -> amount
    }
    (pubKeys.map(k => Algos.encode(k.pubKeyBytes)) -- positiveBalance.keys.map(_._1))
      .map(_ -> Algos.encode(settings.constants.IntrinsicTokenId) -> 0L).toSeq ++ positiveBalance
  }.sortBy(_._1._1 != Algos.encode(accountManagers.head.publicAccounts.head.pubKeyBytes))

  def contractHashesToKeys(pubKeys: Set[PublicKey25519]): Map[String, String] = pubKeys
    .map(key => Algos.encode(key.pubKeyBytes) -> key.address.address)
    .map { case (key, addr) =>
      Algos.encode(EncryProposition.addressLocked(addr).contractHash) -> key
    }.toMap

  private def validateMnemonicKey(mnemonic: String): Either[NonEmptyChain[String], String] = {
    val words: Array[String] = mnemonic.split(" ")
    val isValidSize: Validated[NonEmptyChain[String], String] =
      if (words.length == 12) mnemonic.validNec else "Wrong words size".invalidNec
    val isValidWords: Validated[NonEmptyChain[String], String] =
      if (words.forall(word => Mnemonic.getWords.contains(word))) mnemonic.validNec
      else "Several words don't contain in available words".invalidNec
    (isValidSize, isValidWords).mapN { case (_, mnemonic) => mnemonic }
  }.toEither

  override def close(): Unit = {
    walletStorage.close()
    accountStore.close()
  }
}

object EncryWallet extends StrictLogging {

  def getWalletDir(settings: EncryAppSettings): File =
    if (settings.snapshotSettings.enableFastSynchronization) {
      logger.info(s"Init wallet on nvh with wallet tmp dir")
      new File(s"${settings.directory}/walletTmp")
    } else {
      logger.info(s"Init wallet on nvh with wallet dir")
      new File(s"${settings.directory}/wallet")
    }

  def getKeysDir(settings: EncryAppSettings): File =
    if (settings.snapshotSettings.enableFastSynchronization) {
      logger.info(s"Init wallet on nvh with keys tmp dir")
      new File(s"${settings.directory}/keysTmp")
    } else {
      logger.info(s"Init wallet on nvh with keys dir")
      new File(s"${settings.directory}/keys")
    }

  def scanTree(node: Node[StorageKey, StorageValue],
               storage: VersionalStorage,
               accounts: Set[EncryProposition]): List[EncryBaseBox] = node match {
    case sh: ShadowNode[StorageKey, StorageValue] =>
      val restoredNode = sh.restoreFullNode(storage)
      scanTree(restoredNode, storage, accounts)
    case internalNode: InternalNode[StorageKey, StorageValue] =>
      StateModifierSerializer.parseBytes(internalNode.value, internalNode.key.head) match {
        case Success(bx) => collectBx(bx, accounts) :::
           scanTree(internalNode.leftChild, storage, accounts) :::
           scanTree(internalNode.rightChild, storage, accounts)
        case Failure(exception) => throw exception //???????
      }
    case leafNode: LeafNode[StorageKey, StorageValue] =>
      StateModifierSerializer.parseBytes(leafNode.value, leafNode.key.head) match {
        case Success(bx) => collectBx(bx, accounts)
        case Failure(exception) => throw exception //???????
      }
    case shadowNode: ShadowNode[StorageKey, StorageValue] => List.empty
  }

  def collectBx(box: EncryBaseBox, accounts: Set[EncryProposition]): List[EncryBaseBox] = box match {
    case monetary: MonetaryBox if accounts.exists(_.contractHash sameElements monetary.proposition.contractHash) =>
      List(monetary)
    case _ => List.empty[MonetaryBox]
  }

  def readOrGenerate(walletDir: File, keysDir: File, settings: EncryAppSettings): EncryWallet = {
    walletDir.mkdirs()
    keysDir.mkdirs()
    val db: DB = LevelDbFactory.factory.open(walletDir, new Options)
    val accountManagerStore: LSMStore = new LSMStore(keysDir, keepVersions = 0, keySize = 34) // 34 = 1 prefix byte + 1 account number byte + 32 key bytes
    val walletStorage: WalletVersionalLevelDB = WalletVersionalLevelDBCompanion(db, settings.levelDB)
    val password: String = settings.wallet.map(_.password).getOrElse(throw new RuntimeException("Password not specified"))
    val restoredAccounts: Seq[AccountManager] = AccountManager.restoreAccounts(accountManagerStore, password)
    //init keys
    EncryWallet(walletStorage, restoredAccounts, accountManagerStore)
  }
}