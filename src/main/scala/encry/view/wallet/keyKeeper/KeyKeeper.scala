package encry.view.wallet.keyKeeper

import java.io.File

import scorex.core.transaction.state.PrivateKey25519
import scorex.core.utils.ScorexLogging
import scorex.crypto.signatures.Curve25519

import scala.util.Try

/**
  * KeyKeeper manages LMStore with private keys (Only Pk25519)
 *
  * @param dir
  * @param password
  */

case class KeyKeeper(dir : Option[String],
                    password : String) extends ScorexLogging{

  /**
    * Generate private key from some string bytes
    * @param seed
    * @return
    */
  def generatePKfromStr(seed : String): PrivateKey25519 ={
    val pair = Curve25519.createKeyPair(seed.getBytes())
    PrivateKey25519(pair._1, pair._2)
  }

  /**
    * open KeyKeeper and return set of keys inside store. If store dosn't exist or store was damaged return
    * only one Private key, which was generated from user-app password
    * @return
    */
  def unlock : Set[PrivateKey25519] ={
    val storeFile  =  for {
      maybeFilename <- dir
      file = new File(maybeFilename)
      if file.exists
    } yield file

    storeFile match {
      case None => {
        log.debug("KeyKepper store doesn't exists. Starting with empty KeyKepper. Generate key from user-app password")
        Set(generatePKfromStr(password))
      }
        //TODO : implement
      case Some(file) => {
        Set(generatePKfromStr(password))
      }
    }
  }

  //TODO: implement
  /**
    * Lock KeyKeeper with GOST 34.12-2015 or AES
    */
  def lock() : Try[Unit] = Try()

  //TODO: implement
  def generateSync() : Try[Array[Byte]] = Try(Array(0.toByte))

  //TODO: implement
  /**
    * add Key to store
    */
  def addKey() : Try[Unit] = Try()

  //TODO: implement
  /**
    * delete key from store
    */
  def delKey() : Try[Unit] = Try()

}
