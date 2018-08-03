package encry.view.wallet

import encry.crypto.{PrivateKey25519, PublicKey25519}
import encry.crypto.encryption.AES
import encry.settings.{Algos, WalletSettings}
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import scorex.crypto.hash.Blake2b256
import scorex.crypto.signatures.{Curve25519, PrivateKey, PublicKey}
import scorex.utils.Random

case class AccountManager(store: LSMStore, settings: WalletSettings) {

  def mandatoryAccount: PrivateKey25519 = store.get(AccountManager.MandatoryAccountKey) match {
    case Some(res) => store.get(ByteArrayWrapper(AccountManager.AccountPrefix +: res.data)).map { secretRes =>
      PrivateKey25519(PrivateKey @@ AES.decrypt(secretRes.data, settings.password), PublicKey @@ res.data)
    }.getOrElse(throw new Exception("Mandatory account not found"))
    case None => createMandatory(settings.seed)
  }

  def accounts: Seq[PrivateKey25519] = store.getAll().foldLeft(Seq.empty[PrivateKey25519]) { case (acc, (k, v)) =>
    if (k.data.head == AccountManager.AccountPrefix)
      acc :+ PrivateKey25519(PrivateKey @@ AES.decrypt(v.data, settings.password), PublicKey @@ k.data)
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

  def createMandatory(seedOpt: Option[String]): PrivateKey25519 = {
    val acc: PrivateKey25519 = seedOpt.map(seed => createAccount(seed.getBytes(Algos.charset))).getOrElse(createAccount())
    store.update(
      ByteArrayWrapper(Random.randomBytes(32)),
      Seq.empty,
      Seq((AccountManager.MandatoryAccountKey, ByteArrayWrapper(acc.publicKeyBytes)))
    )
    acc
  }

  private def saveAccount(privateKey: PrivateKey, publicKey: PublicKey): Unit =
    store.update(
      ByteArrayWrapper(Algos.hash(publicKey)),
      Seq.empty,
      Seq((ByteArrayWrapper(AccountManager.AccountPrefix +: publicKey), ByteArrayWrapper(AES.encrypt(privateKey, settings.password))))
    )
}

object AccountManager {
  val AccountPrefix: Byte = 0x05
  val MetaInfoPrefix: Byte = 0x15
  val MandatoryAccountKey: ByteArrayWrapper = ByteArrayWrapper(MetaInfoPrefix +: Algos.hash("account"))
}
