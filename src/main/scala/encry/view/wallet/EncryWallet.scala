package encry.view.wallet

import java.io.File

import cats.implicits._
import com.typesafe.scalalogging.StrictLogging
import encry.settings.EncryAppSettings
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, WalletVersionalLevelDB, WalletVersionalLevelDBCompanion}
import encry.utils.CoreTaggedTypes.VersionTag
import io.iohk.iodb.{LSMStore, Store}
import org.encryfoundation.common.crypto.PublicKey25519
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.modifiers.state.box.{EncryBaseBox, EncryProposition}
import org.encryfoundation.common.utils.TaggedTypes.{ADKey, ModifierId}
import org.iq80.leveldb.{DB, Options}

import scala.util.Try

case class EncryWallet(walletStorage: WalletVersionalLevelDB, accountManagers: Seq[AccountManager], intrinsicTokenId: ADKey, private val accountStore: Store)
  extends StrictLogging with AutoCloseable {

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

  def scanPersistent(modifier: PersistentModifier): Unit = modifier match {
    case block: Block =>
      val (newBxs: Seq[EncryBaseBox], spentBxs: Seq[EncryBaseBox]) =
        block.payload.txs.foldLeft(Seq.empty[EncryBaseBox], Seq.empty[EncryBaseBox]) {
          case ((nBxs, sBxs), tx: Transaction) =>
            val newBxsL: Seq[EncryBaseBox] = tx.newBoxes
              .foldLeft(Seq.empty[EncryBaseBox]) { case (nBxs2, bx) =>
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
      walletStorage.updateWallet(modifier.id, newBxs, spentBxs, intrinsicTokenId)

    case _ => ()
  }

  def rollback(to: VersionTag): Try[Unit] = Try(walletStorage.rollback(ModifierId @@ to.untag(VersionTag)))

  def getBalances: Seq[(String, Long)] = walletStorage.getBalances.toSeq

  override def close(): Unit = walletStorage.close()
}

object EncryWallet extends StrictLogging {

  def getWalletDir(settings: EncryAppSettings): File = new File(s"${settings.directory}/wallet")

  def getKeysDir(settings: EncryAppSettings): File = new File(s"${settings.directory}/keys")

  def readOrGenerate(settings: EncryAppSettings): EncryWallet = {
    val walletDir: File = getWalletDir(settings)
    walletDir.mkdirs()
    val keysDir: File = getKeysDir(settings)
    keysDir.mkdirs()
    val db: DB = LevelDbFactory.factory.open(walletDir, new Options)
    val accountManagerStore: LSMStore = new LSMStore(keysDir, keepVersions = 0, keySize = 34)
    val walletStorage = WalletVersionalLevelDBCompanion(db, settings.levelDB)
    val password: String = settings.wallet.map(_.password).getOrElse(throw new RuntimeException("Password not specified"))
    val accountManager = AccountManager(accountManagerStore, password, settings.wallet.flatMap(_.seed), 0)
    //init keys
    accountManager.mandatoryAccount
    EncryWallet(walletStorage, Seq(accountManager), settings.constants.IntrinsicTokenId, accountManagerStore)
  }
}