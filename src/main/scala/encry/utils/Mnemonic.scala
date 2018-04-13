package encry.utils

import encry.settings.Algos
import scodec.bits.BitVector

import scala.io.Source

object Mnemonic {

  private def getWords(language: String): Array[String] = Source.fromFile("src/main/resources/languages/" + language + "/words.txt").getLines.toArray

  /**
    * Generate seed from mnemonicKey
    * @param mnemonicCode
    * @param passPhrase
    * @return
    */
  //TODO: add passPhrase support
  def mnemonicCodeToBytes(mnemonicCode: String, passPhrase: String = ""): Array[Byte] =
    Algos.hash(mnemonicCode + "mnemonic=" + passPhrase)


  /**
    * Encode entropy to mnemonicSeed
    * @param entropy
    * @return
    */
  //TODO: add support of 256 bits entropy
  def entropyToMnemonicCode(entropy: Array[Byte], language: String = "english"): String = {

    val words = getWords(language)

    val checkSum = BitVector(Algos.hash(entropy))

    val entropyWithCheckSum = BitVector(entropy) ++ checkSum.take(4)

    entropyWithCheckSum.grouped(11).map{ i =>
      words(i.toInt(false))
    }.mkString(" ")

  }
}
