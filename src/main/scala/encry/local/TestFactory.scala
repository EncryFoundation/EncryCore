package encry.local

import java.io.{File, FileWriter}

import encry.crypto.Address
import encry.modifiers.mempool.EncryTransaction.Amount
import encry.modifiers.state.box.proposition.AddressProposition
import encry.modifiers.state.box.{AssetBox, EncryBaseBox}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.PrivateKey25519
import scorex.crypto.authds.ADKey
import scorex.crypto.signatures.Curve25519

import scala.io.Source
import scala.util.Random

// Toolbox for tests only!
object TestFactory {

  object TestProps {
    lazy val keysQty = 100
    lazy val nonce = 0
    lazy val boxValue: Amount = 1000
    lazy val txAmount: Amount = 900
    lazy val txFee: Amount = 100
    lazy val testDir = "test/"
    lazy val keysFilePath = s"${testDir}seeds"
    lazy val recipientAddr: Address = Address @@ "3goCpFrrBakKJwxk7d4oY5HN54dYMQZbmVWKvQBPZPDvbL3hHp"
  }

  // TODO: Should be evaluated once during the test.
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
      genKeysFile(TestProps.keysQty, TestProps.keysFilePath)
      getKeysFromFile
    }
  }

  // TODO: This method is redundant.
  def genProps(privKeys: Seq[PrivateKey25519]): Seq[PublicKey25519Proposition] =
    privKeys.foldLeft(Seq[PublicKey25519Proposition]()) { case (s, pk) =>
      s :+ PublicKey25519Proposition(pk.publicKeyBytes)
    }

  def genAssetBoxes: IndexedSeq[AssetBox] =
    getOrGenerateKeys(TestProps.keysFilePath).foldLeft(IndexedSeq[AssetBox]()) { case (bxs, pk) =>
      bxs :+ AssetBox(
        AddressProposition(Address @@ PublicKey25519Proposition(pk.publicKeyBytes).address),
        TestProps.nonce, TestProps.boxValue)
    }

  def genAssetBox(address: Address): AssetBox =
    AssetBox(AddressProposition(address), TestProps.nonce, TestProps.boxValue)

  def genTxOutputs(boxes: Traversable[EncryBaseBox]): IndexedSeq[ADKey] =
    boxes.foldLeft(IndexedSeq[ADKey]()) { case(s, box) =>
      s :+ box.id
    }

//  // TODO: This method is redundant.
//  def getRandomTxOutputs(amount: Amount): IndexedSeq[(Address, Amount)] = {
//    val div = Random.shuffle(Seq(2, 10, 5, 1)).head
//    (0 until amount.toInt).foldLeft(IndexedSeq[(Address, Amount)]()) { case (outs, am) =>
//      outs :+ (TestProps.recipientAddr, am.toLong)
//    }
//  }
}
