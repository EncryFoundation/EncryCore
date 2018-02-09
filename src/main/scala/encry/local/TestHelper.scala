package encry.local

import java.io.{File, FileWriter}

import encry.account.Address
import encry.modifiers.mempool.EncryTransaction.Amount
import encry.modifiers.state.box.proposition.AddressProposition
import encry.modifiers.state.box.{AssetBox, EncryBaseBox}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.PrivateKey25519
import scorex.crypto.authds.ADKey
import scorex.crypto.signatures.Curve25519

import scala.io.Source
import scala.util.Random

object TestHelper {

  lazy val genesisSeed = Long.MaxValue
  lazy val rndGen = new scala.util.Random(genesisSeed)

  object Props {
    lazy val keysQty = 1000
    lazy val boxValue: Amount = 1000
    lazy val txAmount: Amount = 900
    lazy val txFee: Amount = 100
    lazy val testDir = "test-data/"
    lazy val keysFilePath = s"${testDir}seeds"
    lazy val recipientAddr: Address = Address @@ "3goCpFrrBakKJwxk7d4oY5HN54dYMQZbmVWKvQBPZPDvbL3hHp"
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
        AddressProposition(Address @@ PublicKey25519Proposition(pk.publicKeyBytes).address),
        rndGen.nextLong(), Props.boxValue)
    }

  def genAssetBox(address: Address): AssetBox =
    AssetBox(AddressProposition(address), 9L, Props.boxValue)

  def genTxOutputs(boxes: Traversable[EncryBaseBox]): IndexedSeq[ADKey] =
    boxes.foldLeft(IndexedSeq[ADKey]()) { case(s, box) =>
      s :+ box.id
    }
}
