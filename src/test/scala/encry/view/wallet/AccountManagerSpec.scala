package encry.view.wallet

import encry.crypto.encryption.AES
import encry.view.wallet.AccountManager.MetaInfoPrefix
import io.iohk.iodb.{ByteArrayWrapper, Store}
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
  val seed = Some("seeeeeeed")

  def accountKey(i: Byte): ByteArrayWrapper = ByteArrayWrapper(Array(MetaInfoPrefix, i) ++ Algos.hash("account"))
  def dataKey(i: Byte, pubBytes: PublicKey): ByteArrayWrapper = ByteArrayWrapper(Array(AccountManager.AccountPrefix, i) ++ pubBytes)

  property("Restoring existing accounts from storage") {
    when(storageMock.get(accountKey(0.toByte))).thenReturn(Some(ByteArrayWrapper(accounts(0).publicKeyBytes)))
    when(storageMock.get(accountKey(1.toByte))).thenReturn(Some(ByteArrayWrapper(accounts(1).publicKeyBytes)))
    when(storageMock.get(accountKey(2.toByte))).thenReturn(Some(ByteArrayWrapper(accounts(2).publicKeyBytes)))
    when(storageMock.get(accountKey(3.toByte))).thenReturn(None)

    when(storageMock.get(dataKey(0, accounts(0).publicKeyBytes))).thenReturn(Some(ByteArrayWrapper(AES.encrypt(accounts(0).privKeyBytes, storagePassword))))
    when(storageMock.get(dataKey(1, accounts(1).publicKeyBytes))).thenReturn(Some(ByteArrayWrapper(AES.encrypt(accounts(1).privKeyBytes, storagePassword))))
    when(storageMock.get(dataKey(2, accounts(2).publicKeyBytes))).thenReturn(Some(ByteArrayWrapper(AES.encrypt(accounts(2).privKeyBytes, storagePassword))))

    val restored: Seq[AccountManager] = AccountManager.restoreAccounts(storageMock, storagePassword)
    restored.zipWithIndex.foreach { case (am, i) =>
      assert(
        am.mandatoryAccount.privKeyBytes.sameElements(accounts(i).privKeyBytes) && am.mandatoryAccount.publicKeyBytes.sameElements(accounts(i).publicKeyBytes)
      )
    }

    verify(storageMock, times(7)).get(any[ByteArrayWrapper])
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

}
