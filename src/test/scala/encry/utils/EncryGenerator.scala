package encry.utils

import encry.account.Address
import encry.crypto.{PrivateKey25519, PublicKey25519}
import encry.local.TestHelper.Props
import encry.modifiers.mempool.{EncryTransaction, TransactionFactory}
import encry.modifiers.state.box.proposition.AccountProposition
import encry.modifiers.state.box.{AssetBox, EncryBaseBox}
import scorex.crypto.authds.ADKey
import scorex.crypto.signatures.{Curve25519, PublicKey}
import scorex.utils.Random

trait EncryGenerator {

  def genAssetBox(address: Address): AssetBox =
    AssetBox(AccountProposition(address), 9L, Props.boxValue)

  def genTxOutputs(boxes: Traversable[EncryBaseBox]): IndexedSeq[ADKey] =
    boxes.foldLeft(IndexedSeq[ADKey]()) { case (s, box) =>
      s :+ box.id
    }

  def genPrivKeys(qty: Int): Seq[PrivateKey25519] = (0 until qty).map { _ =>
    val keys = Curve25519.createKeyPair(Random.randomBytes())
    PrivateKey25519(keys._1, keys._2)
  }

  def genValidPaymentTxs(qty: Int): Seq[EncryTransaction] = {
    val pks = genPrivKeys(qty)
    val timestamp = System.currentTimeMillis()

    pks.map { k =>
      val useBoxes = IndexedSeq(genAssetBox(k.publicImage.address))
      TransactionFactory.defaultPaymentTransaction(k, Props.txFee,
        timestamp, useBoxes, Props.recipientAddr, Props.boxValue)
    }
  }

  def genInvalidPaymentTxs(qty: Int): Seq[EncryTransaction] = {
    val pks = genPrivKeys(qty)
    val timestamp = System.currentTimeMillis()

    pks.map { k =>
      val useBoxes = IndexedSeq(genAssetBox(PublicKey25519(PublicKey @@ Random.randomBytes(32)).address))
      TransactionFactory.defaultPaymentTransaction(k, Props.txFee,
        timestamp, useBoxes, Props.recipientAddr, Props.boxValue)
    }
  }
}
