package encry.utils

import encry.account.Address
import encry.local.TestHelper.Props
import encry.modifiers.mempool.PaymentTransaction
import encry.modifiers.state.box.proposition.AddressProposition
import encry.modifiers.state.box.{AssetBox, EncryBaseBox}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.crypto.authds.ADKey
import scorex.crypto.signatures.{Curve25519, PublicKey}
import scorex.utils.Random

trait EncryGenerator {

  def genAssetBox(address: Address): AssetBox =
    AssetBox(AddressProposition(address), 9L, Props.boxValue)

  def genTxOutputs(boxes: Traversable[EncryBaseBox]): IndexedSeq[ADKey] =
    boxes.foldLeft(IndexedSeq[ADKey]()) { case (s, box) =>
      s :+ box.id
    }

  def genPrivKeys(qty: Int): Seq[PrivateKey25519] = (0 until qty).map { _ =>
    val keys = Curve25519.createKeyPair(Random.randomBytes())
    PrivateKey25519(keys._1, keys._2)
  }

  def genValidPaymentTxs(qty: Int): Seq[PaymentTransaction] = {
    val pks = genPrivKeys(qty)
    pks.map { key =>
      val proposition = key.publicImage
      val timestamp = System.currentTimeMillis()
      val useBoxes = IndexedSeq(genAssetBox(Address @@ proposition.address)).map(_.id)
      val outputs = IndexedSeq((Address @@ Props.recipientAddr, Props.boxValue))
      val sig = PrivateKey25519Companion.sign(
        key, PaymentTransaction.getMessageToSign(proposition, Props.txFee, timestamp, useBoxes, outputs))
      PaymentTransaction(proposition, Props.txFee, timestamp, sig, useBoxes, outputs)
    }
  }

  def genInvalidPaymentTxs(qty: Int): Seq[PaymentTransaction] = {
    val pks = genPrivKeys(qty)
    pks.map { key =>
      val proposition = PublicKey25519Proposition(PublicKey @@ Random.randomBytes())
      val timestamp = System.currentTimeMillis()
      val useBoxes = IndexedSeq(genAssetBox(Address @@ proposition.address)).map(_.id)
      val outputs = IndexedSeq((Address @@ Props.recipientAddr, Props.boxValue))
      val sig = PrivateKey25519Companion.sign(
       key, PaymentTransaction.getMessageToSign(proposition, Props.txFee, timestamp, useBoxes, outputs))
      PaymentTransaction(proposition, Props.txFee, timestamp, sig, useBoxes, outputs)
    }
  }
}
