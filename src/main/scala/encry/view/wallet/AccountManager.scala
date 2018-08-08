package encry.view.wallet

import encry.EncryApp
import encry.EncryApp.settings
import encry.crypto.encryption.AES
import encry.crypto.{PrivateKey25519, PublicKey25519}
import encry.settings.Algos
import encry.utils.Logging
import io.iohk.iodb.{ByteArrayWrapper, Store}
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Blake2b256
import scorex.crypto.signatures.{Curve25519, PrivateKey, PublicKey}
import scorex.utils.Random
import scala.util.Try

case class AccountManager(store: Store) extends Logging {

  import encry.storage.EncryStorage._

  def mandatoryAccount: PrivateKey25519 = store.get(AccountManager.MandatoryAccountKey).flatMap { res =>
    store.get(AccountManager.AccountPrefix +: res.data).map { secretRes =>
      PrivateKey25519(PrivateKey @@ decrypt(secretRes.data), PublicKey @@ res.data)
    }
  } getOrElse createMandatory(settings.wallet.seed.flatMap(seed => Base58.decode(seed).toOption))

  def accounts: Seq[PrivateKey25519] = store.getAll().foldLeft(Seq.empty[PrivateKey25519]) { case (acc, (k, v)) =>
    if (k.data.head == AccountManager.AccountPrefix)
      acc :+ PrivateKey25519(PrivateKey @@ decrypt(v.data), PublicKey @@ k.data)
    else acc
  }

  def publicAccounts: Seq[PublicKey25519] = store.getAll().foldLeft(Seq.empty[PublicKey25519]) { case (acc, (k, _)) =>
    if (k.data.head == AccountManager.AccountPrefix) acc :+ PublicKey25519(PublicKey @@ k.data.tail)
    else acc
  }

  def createAccount(seed: Array[Byte] = Random.randomBytes(32)): PrivateKey25519 = {
    val (privateKey: PrivateKey, publicKey: PublicKey) = Curve25519.createKeyPair(Blake2b256.hash(seed))
    saveAccount(privateKey, publicKey)
    PrivateKey25519(privateKey, publicKey)
  }

  def createMandatory(seedOpt: Option[Array[Byte]]): PrivateKey25519 = {
    val acc: PrivateKey25519 = seedOpt
      .map(createAccount)
      .getOrElse(createAccount())
    store.update(
      scala.util.Random.nextLong(),
      Seq.empty,
      Seq((AccountManager.MandatoryAccountKey, ByteArrayWrapper(acc.publicKeyBytes)))
    )
    acc
  }

  private def decrypt(data: Array[Byte]): Array[Byte] = Try(AES.decrypt(data, settings.wallet.password))
    .fold(e => { logError("AccountManager: decryption failed", e.getCause); EncryApp.forceStopApplication(500) }, r => r)

  private def saveAccount(privateKey: PrivateKey, publicKey: PublicKey): Unit =
    store.update(
      scala.util.Random.nextLong(),
      Seq.empty,
      Seq((ByteArrayWrapper(AccountManager.AccountPrefix +: publicKey), ByteArrayWrapper(AES.encrypt(privateKey, settings.wallet.password))))
    )
}

object AccountManager {
  val AccountPrefix: Byte = 0x05
  val MetaInfoPrefix: Byte = 0x15
  val MandatoryAccountKey: ByteArrayWrapper = ByteArrayWrapper(MetaInfoPrefix +: Algos.hash("account"))
}
