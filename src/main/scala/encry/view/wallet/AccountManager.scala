package encry.view.wallet

import com.typesafe.scalalogging.StrictLogging
import encry.EncryApp
import encry.crypto.encryption.AES
import encry.utils.Mnemonic
import io.iohk.iodb.{ByteArrayWrapper, Store}
import org.encryfoundation.common.crypto.{PrivateKey25519, PublicKey25519}
import org.encryfoundation.common.utils.Algos
import scorex.crypto.hash.Blake2b256
import scorex.crypto.signatures.{Curve25519, PrivateKey, PublicKey}

import scala.util.Try

case class AccountManager private(store: Store, password: String, mandatoryAccount: PrivateKey25519, number: Byte) extends StrictLogging {

  def accounts: Seq[PrivateKey25519] = store.getAll().foldLeft(Seq.empty[PrivateKey25519]) { case (acc, (k, v)) =>
    if (k.data.take(2).sameElements(Array(AccountManager.AccountPrefix, number)))
      acc :+ PrivateKey25519(PrivateKey @@ decrypt(v.data), PublicKey @@ k.data)
    else acc
  }

  def publicAccounts: Seq[PublicKey25519] = store.getAll().foldLeft(Seq.empty[PublicKey25519]) { case (acc, (k, _)) =>
    if (k.data.take(2).sameElements(Array(AccountManager.AccountPrefix, number))) acc :+ PublicKey25519(PublicKey @@ k.data.drop(2))
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

  private def decrypt(data: Array[Byte]): Array[Byte] = Try(AES.decrypt(data, password))
    .fold(e => {
      EncryApp.forceStopApplication(500, s"AccountManager: decryption failed cause ${e.getCause}")
    }, r => r)

  private def saveAccount(privateKey: PrivateKey, publicKey: PublicKey): Unit = {
    store.update(
      scala.util.Random.nextLong(),
      Seq.empty,
      Seq((ByteArrayWrapper(
        Array(AccountManager.AccountPrefix, number) ++ publicKey),
        ByteArrayWrapper(AES.encrypt(privateKey, password))
      ))

    )
  }
}

object AccountManager {
  val AccountPrefix: Byte = 0x05
  val MetaInfoPrefix: Byte = 0x15

  def restoreAccounts(store: Store, password: String): Seq[AccountManager] =
    (0.toByte to Byte.MaxValue).foldLeft((Seq.empty[AccountManager], true)) { case ((retrieved, foundLast), number) =>
      if (!foundLast) retrieved -> false
      else {
        val mandatoryAccountKey: ByteArrayWrapper = ByteArrayWrapper(Array(MetaInfoPrefix, number.toByte) ++ Algos.hash("account"))
        val accOpt: Option[PrivateKey25519] = store.get(mandatoryAccountKey).flatMap{ res =>
        val key: ByteArrayWrapper = ByteArrayWrapper(Array(AccountManager.AccountPrefix, number.toByte) ++ res.data)
          store.get(key).map { secretRes =>
            PrivateKey25519(PrivateKey @@ AES.decrypt(secretRes.data, password), PublicKey @@ res.data)
          }
        }

        accOpt match {
          case Some(acc) => (retrieved :+ this(store, password, acc, number.toByte)) -> true
          case None => retrieved -> false
        }
      }
    }._1

  def apply(store: Store, password: String, seedOpt: Option[String], number: Byte): AccountManager = {
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
    saveAccount(store, password, number, privateKey, publicKey)
    this(store, password, PrivateKey25519(privateKey, publicKey), number)
  }

  private def saveAccount(store: Store, password: String, number: Byte, privateKey: PrivateKey, publicKey: PublicKey): Unit = {
    store.update(
      scala.util.Random.nextLong(),
      Seq.empty,
      Seq(
        ByteArrayWrapper(Array(AccountManager.AccountPrefix, number) ++ publicKey) -> ByteArrayWrapper(AES.encrypt(privateKey, password)),
        ByteArrayWrapper(Array(MetaInfoPrefix, number) ++ Algos.hash("account")) -> ByteArrayWrapper(publicKey)
      )
    )
  }
}
