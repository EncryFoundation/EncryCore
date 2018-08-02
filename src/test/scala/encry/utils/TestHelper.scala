package encry.utils

import java.io.{File, FileWriter}
import encry.Address
import encry.crypto.PrivateKey25519
import encry.modifiers.state.box.Box.Amount
import encry.modifiers.state.box.{AssetBox, EncryBaseBox, EncryProposition}
import scorex.crypto.authds.ADKey
import scorex.crypto.signatures.Curve25519
import scala.io.Source
import scala.util.Random

object TestHelper {

  lazy val genesisSeed: Long = Long.MaxValue
  lazy val rndGen = new scala.util.Random(genesisSeed)

  object Props {
    lazy val keysQty = 1000
    lazy val boxValue: Amount = 1000000
    lazy val txAmount: Amount = 199000
    lazy val txFee: Amount = 4300
    lazy val testDir = "test-data/"
    lazy val keysFilePath = s"${testDir}seeds"
  }

  def genKeysFile(qty: Int, filePath: String): Unit = {
    val fileWriter = new FileWriter(new File(filePath))
    (0 until qty).foreach { _ =>
      fileWriter.write(Random.alphanumeric.take(32).mkString + "\n")
    }
    fileWriter.close()
  }

  def getOrGenerateKeys(filePath: String): Seq[PrivateKey25519] = {
    def getKeysFromFile: Seq[PrivateKey25519] =
      Source.fromFile(new File(filePath)).getLines.toList
        .foldLeft(Seq[PrivateKey25519]()) { case (pks, keyString) =>
          val keys = Curve25519.createKeyPair(keyString.getBytes())
          pks :+ PrivateKey25519(keys._1, keys._2)
        }

    val file = new File(filePath)
    if (file.exists) getKeysFromFile
    else {
      genKeysFile(Props.keysQty, Props.keysFilePath)
      getKeysFromFile
    }
  }

  def genAssetBoxes: IndexedSeq[AssetBox] =
    getOrGenerateKeys(Props.keysFilePath).foldLeft(IndexedSeq[AssetBox]()) { case (bxs, pk) =>
      bxs :+ AssetBox(
        EncryProposition.pubKeyLocked(pk.publicKeyBytes),
        rndGen.nextLong(), Props.boxValue)
    }

  def genAssetBox(address: Address, amount: Amount = 9L): AssetBox =
    AssetBox(EncryProposition.addressLocked(address), amount, Props.boxValue)

  def genTxOutputs(boxes: Traversable[EncryBaseBox]): IndexedSeq[ADKey] =
    boxes.foldLeft(IndexedSeq[ADKey]()) { case(s, box) =>
      s :+ box.id
    }
}
