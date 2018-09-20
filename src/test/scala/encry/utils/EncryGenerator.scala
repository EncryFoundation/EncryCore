package encry.utils

import java.io.File

import akka.actor.ActorRef
import encry.avltree._
import encry.crypto.equihash.EquihashSolution
import encry.modifiers.history.Header
import encry.modifiers.mempool.{Transaction, TransactionFactory}
import encry.modifiers.state.box.Box.Amount
import encry.modifiers.state.box.{AssetBox, EncryBaseBox, EncryProposition, MonetaryBox}
import encry.settings.Constants
import encry.utils.CoreTaggedTypes.ModifierId
import encry.utils.TestHelper.Props
import encry.view.state.{BoxHolder, EncryState, UtxoState}
import io.iohk.iodb.LSMStore
import org.encryfoundation.common.Algos
import org.encryfoundation.common.Algos.HF
import org.encryfoundation.common.crypto.{PrivateKey25519, PublicKey25519}
import org.encryfoundation.common.transaction.EncryAddress.Address
import org.encryfoundation.common.transaction.Pay2PubKeyAddress
import org.encryfoundation.common.utils.TaggedTypes.{ADDigest, ADKey, ADValue}
import scorex.crypto.hash.Digest32
import scorex.crypto.signatures.{Curve25519, PrivateKey, PublicKey}
import scorex.utils.Random

import scala.util.{Random => ScRand}

trait EncryGenerator {

  def timestamp: Long = System.currentTimeMillis()

  def randomAddress: Address = Pay2PubKeyAddress(PublicKey @@ Random.randomBytes()).address

  def genAssetBox(address: Address, amount: Amount = 100000L, tokenIdOpt: Option[ADKey] = None): AssetBox =
    AssetBox(EncryProposition.addressLocked(address), ScRand.nextLong(), amount, tokenIdOpt)

  def genTxOutputs(boxes: Traversable[EncryBaseBox]): IndexedSeq[ADKey] =
    boxes.foldLeft(IndexedSeq[ADKey]()) { case (s, box) =>
      s :+ box.id
    }

  def genValidAssetBoxes(secret: PrivateKey25519, amount: Amount, qty: Int): Seq[AssetBox] =
    (0 to qty).foldLeft(IndexedSeq[AssetBox]()) { case (bxs, _) =>
      bxs :+ AssetBox(EncryProposition.pubKeyLocked(secret.publicKeyBytes), ScRand.nextLong(), amount)
    }

  def genPrivKeys(qty: Int): Seq[PrivateKey25519] = (0 until qty).map { _ =>
    val keys: (PrivateKey, PublicKey) = Curve25519.createKeyPair(Random.randomBytes())
    PrivateKey25519(keys._1, keys._2)
  }

  def genValidPaymentTxs(qty: Int): Seq[Transaction] = {
    val keys: Seq[PrivateKey25519] = genPrivKeys(qty)
    val now = System.currentTimeMillis()

    keys.map { k =>
      val useBoxes: IndexedSeq[AssetBox] = IndexedSeq(genAssetBox(k.publicImage.address.address))
      TransactionFactory.defaultPaymentTransactionScratch(k, Props.txFee,
        now + scala.util.Random.nextInt(5000), useBoxes, randomAddress, Props.boxValue)
    }
  }

  def genValidPaymentTxsToAddr(qty: Int, address: Address): Seq[Transaction] = {
    val keys: Seq[PrivateKey25519] = genPrivKeys(qty)

    keys.map { k =>
      val useBoxes = IndexedSeq(genAssetBox(address))
      TransactionFactory.defaultPaymentTransactionScratch(k, Props.txFee,
        scala.util.Random.nextLong(), useBoxes, address, Props.boxValue)
    }
  }

  def genValidPaymentTxToAddrWithSpentBoxes(boxes: IndexedSeq[AssetBox], address: Address): Transaction = {
    val key: PrivateKey25519 = genPrivKeys(1).head

    TransactionFactory.defaultPaymentTransactionScratch(key, Props.txFee,
      scala.util.Random.nextLong(), boxes, address, Props.boxValue)
  }

  def genValidPaymentTxsToAddrWithDiffTokens(qty: Int, address: Address): Seq[Transaction] = {
    val keys: Seq[PrivateKey25519] = genPrivKeys(qty)
    val tokens: Seq[ADKey] = (0 until qty).foldLeft(Seq[ADKey]()) {
      case (seq, _) => seq :+ (ADKey @@ Random.randomBytes())
    }
    val pksZipTokens: Seq[(PrivateKey25519, ADKey)] = keys.zip(tokens)
    val timestamp: Amount = System.currentTimeMillis()
    pksZipTokens.map { k =>
      val useBoxes = IndexedSeq(genAssetBox(address, tokenIdOpt = Some(k._2)))
      TransactionFactory.defaultPaymentTransactionScratch(k._1, Props.txFee,
        timestamp, useBoxes, address, Props.boxValue, tokenIdOpt = Some(k._2))
    }
  }

  def genChainSpendingTxs(qty: Int): Seq[Transaction] = {

    val keys: Seq[PrivateKey25519] = genPrivKeys(qty)
    val timestamp: Amount = System.currentTimeMillis()
    keys.foldLeft(Seq[Transaction]()) { (seq, key) =>
      val useBoxes: IndexedSeq[MonetaryBox] = if (seq.isEmpty) IndexedSeq(genAssetBox(key.publicImage.address.address))
      else seq.last.newBoxes.map(_.asInstanceOf[MonetaryBox]).toIndexedSeq
      seq :+ TransactionFactory.defaultPaymentTransactionScratch(key, Props.txFee,
        timestamp, useBoxes, randomAddress, Props.boxValue)
    }
  }


  def genInvalidPaymentTxs(qty: Int): Seq[Transaction] = {
    val timestamp: Amount = System.currentTimeMillis()
    genPrivKeys(qty).map { key =>
      val useBoxes: IndexedSeq[AssetBox] =
        IndexedSeq(genAssetBox(PublicKey25519(PublicKey @@ Random.randomBytes(32)).address.address))
      TransactionFactory.defaultPaymentTransactionScratch(key, -100, timestamp, useBoxes, randomAddress, Props.boxValue)
    }
  }

  def genHeader: Header = {
    val random = new scala.util.Random
    Header(
      1.toByte,
      ModifierId @@ Random.randomBytes(),
      Digest32 @@ Random.randomBytes(),
      ADDigest @@ Random.randomBytes(33),
      Digest32 @@ Random.randomBytes(),
      random.nextLong(),
      random.nextInt(100000),
      random.nextLong(),
      Constants.Chain.InitialDifficulty,
      EquihashSolution(Seq(1, 3))
    )
  }
}
