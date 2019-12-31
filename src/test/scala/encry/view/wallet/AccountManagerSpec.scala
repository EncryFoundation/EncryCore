package encry.view.wallet

import encry.crypto.encryption.AES
import encry.utils.{FileHelper, Mnemonic}
import encry.view.wallet.AccountManager.MetaInfoPrefix
import io.iohk.iodb.{ByteArrayWrapper, LSMStore, Store}
import org.encryfoundation.common.crypto.PrivateKey25519
import org.encryfoundation.common.utils.Algos
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{Matchers, PropSpec}
import org.mockito.Mockito._
import org.mockito.ArgumentMatchers.{any, eq => eq_}
import scorex.crypto.hash.Blake2b256
import scorex.crypto.signatures.{Curve25519, PrivateKey, PublicKey}

import scala.collection.immutable

class AccountManagerSpec extends PropSpec with Matchers with MockitoSugar {

  val storageMock: Store = mock[Store]
  val storagePassword = "0000"
  val accounts: immutable.IndexedSeq[PrivateKey25519] = (1 to 3).map { _ =>
    val seed: Array[Byte] = scorex.utils.Random.randomBytes(16)
    val (privateKey: PrivateKey, publicKey: PublicKey) = Curve25519.createKeyPair(Blake2b256(seed))
    PrivateKey25519(privateKey, publicKey)
  }
  val seed = "seeeeeeed"
  val alsoSeed = "seeed"

  def accountKey(i: Byte): ByteArrayWrapper = ByteArrayWrapper(Array(MetaInfoPrefix, i) ++ Algos.hash("account"))
  def dataKey(i: Byte, pubBytes: PublicKey): ByteArrayWrapper = ByteArrayWrapper(Array(AccountManager.AccountPrefix, i) ++ pubBytes)

  property("Restoring existing accounts from storage") {

    val dir = FileHelper.getRandomTempDir
    val accountManagerStore: LSMStore = new LSMStore(dir, keepVersions = 0, keySize = 34)

    val accountManagerOne = AccountManager(accountManagerStore, storagePassword, seed, 0.toByte)
    val accountManagerTwo = AccountManager(accountManagerStore, storagePassword, alsoSeed, 1.toByte)

    val numberOfAccountToCreate = 3

    (1 to numberOfAccountToCreate).foreach(i => accountManagerOne.createAccount(Some(i.toString)))
    (5 to 4 + numberOfAccountToCreate).foreach(i => accountManagerTwo.createAccount(Some(i.toString)))

    val restoredAccounts = AccountManager.restoreAccounts(accountManagerStore, storagePassword)
    assert(restoredAccounts(0).accounts.size == numberOfAccountToCreate + 1)
    assert(restoredAccounts(1).accounts.size == numberOfAccountToCreate + 1)

    restoredAccounts(0)
      .accounts
      .sortBy(a => Algos.encode(a.privKeyBytes))
      .zip(accountManagerOne.accounts.sortBy(a => Algos.encode(a.privKeyBytes)))
      .foreach { case (restored, created) =>
        assert(restored.publicKeyBytes.sameElements(created.publicKeyBytes) && restored.privKeyBytes.sameElements(created.privKeyBytes))
      }

    restoredAccounts(1)
      .accounts
      .sortBy(a => Algos.encode(a.privKeyBytes))
      .zip(accountManagerTwo.accounts.sortBy(a => Algos.encode(a.privKeyBytes)))
      .foreach { case (restored, created) =>
        assert(restored.publicKeyBytes.sameElements(created.publicKeyBytes) && restored.privKeyBytes.sameElements(created.privKeyBytes))
      }

    accountManagerStore.close()
  }

  property("Creating account from seed") {
    val account: AccountManager = AccountManager(storageMock, storagePassword, seed, 0.toByte)

    val keyToStore =
      ByteArrayWrapper(Array(AccountManager.AccountPrefix, 0.toByte) ++ account.mandatoryAccount.publicKeyBytes) ->
        ByteArrayWrapper(AES.encrypt(account.mandatoryAccount.privKeyBytes, storagePassword))
    val dataToStore =
      ByteArrayWrapper(Array(MetaInfoPrefix, 0.toByte) ++ Algos.hash("account")) -> ByteArrayWrapper(account.mandatoryAccount.publicKeyBytes)

    verify(storageMock, times(1))
      .update(any[Long], eq_(Iterable.empty[ByteArrayWrapper]), eq_(Iterable(keyToStore, dataToStore)))
  }

  property("Create new accounts") {
    val account: AccountManager = AccountManager(storageMock, storagePassword, seed, 0.toByte)
    account.createAccount(Some(seed))

    val keyToStore =
      ByteArrayWrapper(Array(AccountManager.AccountPrefix, 0.toByte) ++ account.mandatoryAccount.publicKeyBytes) ->
        ByteArrayWrapper(AES.encrypt(account.mandatoryAccount.privKeyBytes, storagePassword))

    verify(storageMock, times(1))
      .update(any[Long], eq_(Iterable.empty[ByteArrayWrapper]), eq_(Iterable(keyToStore)))
  }

  property("Correctly create, save and retrieve accounts") {
      val dir = FileHelper.getRandomTempDir
      val accountManagerStore: LSMStore = new LSMStore(dir, keepVersions = 0, keySize = 34)

      val numberOfAccountToCreate = 3
      val accountManager = AccountManager(accountManagerStore, storagePassword, seed, 0.toByte)

      val (privateKeyFromSeed: PrivateKey, publicKeyFromSeed: PublicKey) = Curve25519.createKeyPair(Blake2b256(Mnemonic.seedFromMnemonic(seed)))

      val createdAccounts = (1 to numberOfAccountToCreate).map { i =>
        accountManager.createAccount(Some(i.toString))
      }

      val accountsFromAccountManager = accountManager.accounts

      accountsFromAccountManager
        .sortBy(a => Algos.encode(a.privKeyBytes))
        .zip((PrivateKey25519(privateKeyFromSeed, publicKeyFromSeed) +: createdAccounts).sortBy(a => Algos.encode(a.privKeyBytes)))
        .foreach { case (fromAM, created) =>
          assert(fromAM.publicKeyBytes.sameElements(created.publicKeyBytes) && fromAM.privKeyBytes.sameElements(created.privKeyBytes))
        }

      assert(accountsFromAccountManager.size == numberOfAccountToCreate + 1)

      accountManagerStore.close()
    }

}
