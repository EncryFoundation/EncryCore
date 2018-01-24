package encry.view.wallet.keys

import java.io.File

import com.google.common.primitives.Ints
import encry.settings.{Algos, EncryAppSettings, KeyManagerSettings}
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import scorex.core.transaction.state.PrivateKey25519
import scorex.core.utils.ScorexLogging
import scorex.crypto.hash.{Blake2b512, Digest32}
import scorex.crypto.signatures.Curve25519

import scala.language.postfixOps
import scala.util.Try

/**
  * KeyKeeperStorage manages LMStore with private keys (Only Pk25519)
 *
  * @param store - KeyKeeperStorage storage
  * @param password - password to unlock storage
  */

case class KeyManager(store: LSMStore,
                      storageSettings: KeyManagerSettings,
                      password: Option[String]) extends ScorexLogging {
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
  private def getKeysFromStorageWithChainCode: Seq[(PrivateKey25519, Array[Byte])] = {

    val keysQty =
      store.get(new ByteArrayWrapper(Algos.hash("count"))).map(d => Ints.fromByteArray(d.data)).getOrElse(0)

    (0 until keysQty).foldLeft(Seq[(PrivateKey25519, Array[Byte])]()) {
      case (seq, _) =>
        if (seq.nonEmpty) seq :+ deriveNextKey(seq.last._1, seq.last._2)
        else seq :+ deriveKeysFromSeed(store.get(new ByteArrayWrapper(Algos.hash("seed"))).get.data)
    }

  }

  def keys: Seq[PrivateKey25519] =
    getKeysFromStorageWithChainCode.foldLeft(Seq[PrivateKey25519]()) {
      case (seq, elem) => seq :+ elem._1
    }

  def isEmpty: Boolean = keys.isEmpty

  /**
    * open KeyKeeperStorage and return set of keys inside store. If store dosn't exist or store was damaged return
    * only one key seq, which was generated from user-app password
    * @return
    */
  def unlock(): Unit = {
    store.get(new ByteArrayWrapper(Algos.hash("seed"))) match {
      case Some(_) =>
    }
  }

  /**
    * Lock KeyKeeperStorage with GOST 34.12-2015 or AES
    */
  def lock(): Unit = ???

  def generateSync(): Try[Array[Byte]] = ???

  def addKey(): Unit = {
    val keysQty =
      store.get(new ByteArrayWrapper(Algos.hash("count"))).map(d => Ints.fromByteArray(d.data)).getOrElse(0)
    store.update(System.currentTimeMillis(), Seq(new ByteArrayWrapper(Algos.hash("count"))), Seq())
    store.update(System.currentTimeMillis(), Seq(), Seq(
      (new ByteArrayWrapper(Algos.hash("count")), new ByteArrayWrapper(Ints.toByteArray(keysQty)))
    ))
  }

  /**
    * delete key from store
    */
  def delKey(): Try[Unit] = ???

  def initStorage(seed: Array[Byte]): Unit = {
    store.update(System.currentTimeMillis(),
      Seq(),
      Seq((new ByteArrayWrapper(Algos.hash("seed")), new ByteArrayWrapper(seed)),
        (new ByteArrayWrapper(Algos.hash("count")), new ByteArrayWrapper(Ints.toByteArray(1)))
      )
    )
  }
}

object KeyManager extends ScorexLogging {

  def keysDir(settings: EncryAppSettings) = new File(s"${settings.directory}/keys")

  def readOrGenerate(settings: EncryAppSettings, password: Option[String] = None): KeyManager = {
    val dir = keysDir(settings)
    dir.mkdirs()

    KeyManager(new LSMStore(dir, 32), settings.keyManagerSettings, password)
  }
}
