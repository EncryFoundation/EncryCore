package encry.view.wallet.keys

import java.io.File
import java.security.{AlgorithmParameters, SecureRandom}
import javax.crypto._
import javax.crypto.spec.{IvParameterSpec, PBEKeySpec, SecretKeySpec}

import com.google.common.primitives.{Ints, Longs}
import encry.settings.{Algos, EncryAppSettings, KeyManagerSettings}
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import scorex.core.transaction.state.PrivateKey25519
import scorex.core.utils.ScorexLogging
import scorex.crypto.hash.{Blake2b512, Digest32}
import scorex.crypto.signatures.Curve25519
import scorex.utils.Random

import scala.language.postfixOps
import scala.util.Try


/**
  * KeyKeeperStorage manages LMStore with private keys (Only Pk25519)
 *
  * @param store - KeyKeeperStorage storage
  * @param passwdBytes - password to unlock storage
  */

case class KeyManager(store: LSMStore,
                      storageSettings: KeyManagerSettings,
                      passwdBytes: Option[Array[Byte]]) extends ScorexLogging {
  /**
    * Generate private key from some string bytes
    * @param seed
    * @return Key pair based on seed and chain code
    */
  //TODO: add generateFrom mnemonic key
  def deriveKeysFromSeed(seed: Array[Byte]): (PrivateKey25519, Array[Byte]) = {
    val seedHashBytes = Blake2b512.hash(seed)
    val hashSeq = seedHashBytes.sliding(32).toSeq
    val pair = Curve25519.createKeyPair(hashSeq.head)
    PrivateKey25519(pair._1, pair._2) -> hashSeq(1)
  }

  /**
    * Generate next key based on previous key
    * @param prevKey
    * @return Key pair based on previous key and chain code
    */
  def deriveNextKey(prevKey: (PrivateKey25519, Array[Byte])): (PrivateKey25519, Array[Byte]) = {
    val prevKeyHash = Blake2b512.hash(prevKey._1.publicKeyBytes ++ prevKey._2)
    val hashSeq = prevKeyHash.sliding(32).toSeq
    val pair = Curve25519.createKeyPair(hashSeq.head)
    PrivateKey25519(pair._1, pair._2) -> hashSeq(1)
  }

  /**
    * get hash of  Keys sequence
    * @param keysSeq
    */
  def keysHash(keysSeq: Seq[PrivateKey25519]): Digest32 = Algos.hash(keysSeq.foldLeft(Array[Byte]()) {
    case(currentHash,key) => currentHash ++ Algos.hash(key.publicKeyBytes)
  })

  /**
    * Generate keys from seed and keysHash
    * @return Sequence of keys
    */
  private def getKeysWithChainCode: Seq[(PrivateKey25519, Array[Byte])] = {

    val keysQty =
      store.get(new ByteArrayWrapper(Algos.hash("count"))).map(d => Ints.fromByteArray(d.data)).getOrElse(0)

    (0 until keysQty).foldLeft(Seq[(PrivateKey25519, Array[Byte])]()) {
      case (seq, _) =>
        if (seq.nonEmpty) seq :+ deriveNextKey(seq.last._1, seq.last._2)
        else seq :+ deriveKeysFromSeed(store.get(new ByteArrayWrapper(Algos.hash("seed"))).get.data)
    }
  }

  def updateKey(key: ByteArrayWrapper, newValue: Array[Byte]): Unit = {
    //delete previous value
    store.update(
      new ByteArrayWrapper(Algos.hash(newValue ++ Longs.toByteArray(System.currentTimeMillis()))), Seq(key), Seq()
    )
    //put new value
    store.update(
      new ByteArrayWrapper(Algos.hash(Algos.hash(newValue ++ Longs.toByteArray(System.currentTimeMillis())))), Seq(), Seq((key, new ByteArrayWrapper(newValue)))
    )
  }

  def isLocked: Boolean = (1: Byte) == store.get(KeyManager.lockKey).map(_.data).getOrElse(Array(0: Byte)).head

  def getKey(key: ByteArrayWrapper): Array[Byte] =
    store.get(key).map(_.data).getOrElse(Array[Byte](0))

  def keys: Seq[PrivateKey25519] = {
    if (!isLocked) {
      getKeysWithChainCode.foldLeft(Seq[PrivateKey25519]()) {
        case (seq, elem) => seq :+ elem._1
      }
    }else{
      unlock()
      val keys = getKeysWithChainCode.foldLeft(Seq[PrivateKey25519]()) {
        case (seq, elem) => seq :+ elem._1
      }
      lock()
      keys
    }

  }

  def createNewKey(): Unit = {
    val newKeysQty = store.get(KeyManager.countKey).map(d => Ints.fromByteArray(d.data)).getOrElse(0) + 1
    updateKey(KeyManager.countKey, Ints.toByteArray(newKeysQty))
  }

  /**
    * open KeyKeeperStorage and return set of keys inside store. If store dosn't exist or store was damaged return
    * only one key seq, which was generated from user-app password
    * @return
    */
  def unlock(key: Array[Byte] = passwdBytes.getOrElse(Array[Byte]())): Unit = {
    updateKey(KeyManager.seedKey ,decryptAES(key))
    updateKey(KeyManager.lockKey, KeyManager.unlockFlag)
  }

  /**
    * Lock KeyKeeperStorage with GOST 34.12-2015 or AES
    */
  def lock(key: Array[Byte] = passwdBytes.getOrElse(Array[Byte]())): Unit = {
    val (encryptSeed, iv, salt) = encryptAES(key)
    updateKey(KeyManager.seedKey ,encryptSeed)
    updateKey(KeyManager.ivKey ,iv)
    updateKey(KeyManager.saltKey ,salt)
    updateKey(KeyManager.lockKey, KeyManager.lockFlag)
  }

  def generateSalt: Array[Byte] = {
    val random = new SecureRandom()
    val bytes = new Array[Byte](256/8)
    random.nextBytes(bytes)
    bytes
  }

  /**
    * Encrypt seed with key
    * @param key
    * @return encrypted Seed and IV
    */
  def encryptAES(key: Array[Byte]): (Array[Byte], Array[Byte], Array[Byte]) = {

    val seed = store.get(KeyManager.seedKey).map(_.data).getOrElse(Array[Byte]())

    val saltBytes: Array[Byte] = generateSalt

    // Derive the key
    val factory: SecretKeyFactory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256")

    val spec: PBEKeySpec = new PBEKeySpec(key.map(_.asInstanceOf[Char]), saltBytes, 1000, 128)

    val secretKey: SecretKey = factory.generateSecret(spec)
    val secret: SecretKeySpec = new SecretKeySpec(secretKey.getEncoded, "AES")

    //encrypt the message
    val cipher: Cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
    cipher.init(Cipher.ENCRYPT_MODE, secret)
    val params: AlgorithmParameters = cipher.getParameters
    val ivBytes = params.getParameterSpec(classOf[IvParameterSpec]).getIV
    val encryptedTextBytes: Array[Byte] = cipher.doFinal(seed)

    (encryptedTextBytes, ivBytes, saltBytes)
  }

  /**
    * Decrypt seed with key
    * @param key
    * @return decrypted Seed
    */
  def decryptAES(key: Array[Byte]): Array[Byte] = {


    val saltBytes = store.get(KeyManager.saltKey).map(_.data).getOrElse(Array[Byte]())
    val ivBytes = store.get(KeyManager.ivKey).map(_.data).getOrElse(Array[Byte]())
    val encryptedTextBytes = store.get(KeyManager.seedKey).map(_.data).getOrElse(Array[Byte]())

    // Derive the key
    val factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256")
    val spec = new PBEKeySpec(key.map(_.asInstanceOf[Char]), saltBytes, 1000, 128)

    val secretKey = factory.generateSecret(spec)
    val secret = new SecretKeySpec(secretKey.getEncoded, "AES")

    // Decrypt the message
    val cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
    cipher.init(Cipher.DECRYPT_MODE, secret, new IvParameterSpec(ivBytes))


    var decryptedTextBytes: Array[Byte] = Array[Byte](32)
    try
      decryptedTextBytes = cipher.doFinal(encryptedTextBytes)
    catch {
      case e: IllegalBlockSizeException =>
        e.printStackTrace()
      case e: BadPaddingException =>
        e.printStackTrace()
    }
    decryptedTextBytes
  }

  /**
    * delete key from store
    */
  def delKey(): Try[Unit] = ???

  def initStorage(seed: Array[Byte]): Unit = {

    store.update(System.currentTimeMillis(),
      Seq(),
      Seq((KeyManager.seedKey, new ByteArrayWrapper(seed)),
        (KeyManager.ivKey, new ByteArrayWrapper(Random.randomBytes(0))),
        (KeyManager.saltKey, new ByteArrayWrapper(Random.randomBytes(0))),
        (KeyManager.lockKey, new ByteArrayWrapper(Array(0:Byte))),
        (KeyManager.countKey, new ByteArrayWrapper(Ints.toByteArray(1)))
      )
    )
  }
}

object KeyManager extends ScorexLogging {

  val lockFlag = Array(0: Byte)

  val unlockFlag = Array(1: Byte)

  val seedKey = new ByteArrayWrapper(Algos.hash("seed"))

  val lockKey = new ByteArrayWrapper(Algos.hash("lock"))

  val ivKey = new ByteArrayWrapper(Algos.hash("iv"))

  val saltKey = new ByteArrayWrapper(Algos.hash("salt"))

  val countKey = new ByteArrayWrapper(Algos.hash("count"))

  def getKeysDir(settings: EncryAppSettings) = new File(s"${settings.directory}/keys")

  def readOrGenerate(settings: EncryAppSettings,
                     password: Option[Array[Byte]] = Option(Array[Byte]()),
                     seed: Array[Byte] = Random.randomBytes()): KeyManager = {

    val dir = getKeysDir(settings)
    dir.mkdirs()

    val keysStore = new LSMStore(dir, keepVersions = 0)

    val keyManager = KeyManager(keysStore, settings.keyManagerSettings, password)

    if (keyManager.keys.isEmpty) {
      keyManager.initStorage(seed)
      if (settings.keyManagerSettings.lock && !keyManager.isLocked) {
        keyManager.lock()
      }
    }

    keyManager
  }

}
