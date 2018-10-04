package encry.utils

import org.encryfoundation.common.Algos
import scodec.bits.BitVector
import scala.io.Source

object Mnemonic {

  def seedFromMnemonic(mnemonicCode: String, passPhrase: String = ""): Array[Byte] =
    Algos.hash(mnemonicCode + "mnemonic=" + passPhrase)

  def entropyToMnemonicCode(entropy: Array[Byte]): String = {
    val words: Array[String] =
      Source.fromInputStream(getClass.getResourceAsStream("/languages/english/words.txt")).getLines.toArray
    val checkSum: BitVector = BitVector(Algos.hash(entropy))
    val entropyWithCheckSum: BitVector = BitVector(entropy) ++ checkSum.take(4)

    entropyWithCheckSum.grouped(11).map { i =>
      words(i.toInt(signed = false))
    }.mkString(" ")
  }
}
