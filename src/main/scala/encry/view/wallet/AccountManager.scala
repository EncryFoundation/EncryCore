package encry.view.wallet

import com.typesafe.scalalogging.StrictLogging
import encry.EncryApp
import encry.crypto.encryption.AES
import encry.settings.EncryAppSettings
import encry.utils.Mnemonic
import io.iohk.iodb.{ByteArrayWrapper, Store}
import org.encryfoundation.common.crypto.{PrivateKey25519, PublicKey25519}
import org.encryfoundation.common.utils.Algos
import scorex.crypto.hash.Blake2b256
import scorex.crypto.signatures.{Curve25519, PrivateKey, PublicKey}
import scala.util.Try

case class AccountManager(store: Store) extends StrictLogging {

  import encry.storage.EncryStorage._

  val settings: EncryAppSettings = EncryAppSettings.settings

  lazy val mandatoryAccount: PrivateKey25519 = store.get(AccountManager.MandatoryAccountKey).flatMap { res =>
    store.get(AccountManager.AccountPrefix +: res.data).map { secretRes =>
      PrivateKey25519(PrivateKey @@ decrypt(secretRes.data), PublicKey @@ res.data)
    }
  } getOrElse createMandatory(settings.wallet.flatMap(_.seed))

  def accounts: Seq[PrivateKey25519] = store.getAll().foldLeft(Seq.empty[PrivateKey25519]) { case (acc, (k, v)) =>
    if (k.data.head == AccountManager.AccountPrefix)
      acc :+ PrivateKey25519(PrivateKey @@ decrypt(v.data), PublicKey @@ k.data)
    else acc
  }

  def publicAccounts: Seq[PublicKey25519] = store.getAll().foldLeft(Seq.empty[PublicKey25519]) { case (acc, (k, _)) =>
    if (k.data.head == AccountManager.AccountPrefix) acc :+ PublicKey25519(PublicKey @@ k.data.tail)
    else acc
  }

  def createAccount(seedOpt: Option[String]): PrivateKey25519 = {
    val (privateKey: PrivateKey, publicKey: PublicKey) = Curve25519.createKeyPair(
      Blake2b256.hash(
        seedOpt
          .map {
            Mnemonic.seedFromMnemonic(_)
          }
          .getOrElse {
            val phrase: String = Mnemonic.entropyToMnemonicCode(scorex.utils.Random.randomBytes(16))
            println(s"\nMnemonic code is:\n$phrase")
            Mnemonic.seedFromMnemonic(phrase)
          }
      )
    )
    saveAccount(privateKey, publicKey)
    PrivateKey25519(privateKey, publicKey)
  }

  def createMandatory(seedOpt: Option[String]): PrivateKey25519 = {
    val acc: PrivateKey25519 = createAccount(seedOpt)
    store.update(
      scala.util.Random.nextLong(),
      Seq.empty,
      Seq((AccountManager.MandatoryAccountKey, ByteArrayWrapper(acc.publicKeyBytes)))
    )
    acc
  }

  private def decrypt(data: Array[Byte]): Array[Byte] = Try(AES.decrypt(data, settings.wallet.map(_.password)
    .getOrElse(throw new RuntimeException("password not specified"))))
    .fold(e => {
      EncryApp.forceStopApplication(500, s"AccountManager: decryption failed cause ${e.getCause}")
    }, r => r)

  private def saveAccount(privateKey: PrivateKey, publicKey: PublicKey): Unit = {
    store.update(
      scala.util.Random.nextLong(),
      Seq.empty,

      Seq((ByteArrayWrapper(AccountManager.AccountPrefix +: publicKey),
        ByteArrayWrapper(AES.encrypt(privateKey, settings.wallet.map(_.password)
          .getOrElse(throw new RuntimeException("password not specified"))))))

    )
  }
}

object AccountManager {
  val AccountPrefix: Byte = 0x05
  val MetaInfoPrefix: Byte = 0x15
  val MandatoryAccountKey: ByteArrayWrapper = ByteArrayWrapper(MetaInfoPrefix +: Algos.hash("account"))
}
