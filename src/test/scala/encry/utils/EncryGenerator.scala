package encry.utils

import encry.account.Address
import encry.consensus.Difficulty
import encry.crypto.{PrivateKey25519, PublicKey25519}
import encry.local.TestHelper.Props
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.mempool.{EncryTransaction, TransactionFactory}
import encry.modifiers.state.box.proof.Signature25519
import encry.modifiers.state.box.proposition.AccountProposition
import encry.modifiers.state.box.{AssetBox, EncryBaseBox, MonetaryBox}
import scorex.core.ModifierId
import scorex.core.transaction.box.Box.Amount
import scorex.crypto.authds.{ADDigest, ADKey}
import scorex.crypto.hash.Digest32
import scorex.crypto.signatures.{Curve25519, PublicKey, Signature}
import scorex.utils.Random

trait EncryGenerator {

  def genAssetBox(address: Address, amount: Amount = 9L, tokenIdOpt: Option[ADKey] = None): AssetBox =
    AssetBox(AccountProposition(address), amount, Props.boxValue, tokenIdOpt)


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
      TransactionFactory.defaultPaymentTransactionScratch(k, Props.txFee,
        timestamp, useBoxes, Props.recipientAddr, Props.boxValue)
    }
  }

  def genValidPaymentTxsToAddr(qty: Int, address: Address): Seq[EncryTransaction] = {
    val pks = genPrivKeys(qty)
    val timestamp = System.currentTimeMillis()

    pks.map { k =>
      val useBoxes = IndexedSeq(genAssetBox(address))
      TransactionFactory.defaultPaymentTransactionScratch(k, Props.txFee,
        timestamp, useBoxes, address, Props.boxValue)
    }
  }

  def genValidPaymentTxsToAddrWithDiffTokens(qty: Int, address: Address): Seq[EncryTransaction] = {
    val pks = genPrivKeys(qty)
    val tokens = (0 until qty).foldLeft(Seq[ADKey]()){
      case (seq, _) => seq :+ (ADKey @@ Random.randomBytes())
    }
    val pksZipTokens = pks.zip(tokens)
    val timestamp = System.currentTimeMillis()
    pksZipTokens.map { k =>
      val useBoxes = IndexedSeq(genAssetBox(address, tokenIdOpt = Some(k._2)))
      TransactionFactory.defaultPaymentTransactionScratch(k._1, Props.txFee,
        timestamp, useBoxes, address, Props.boxValue, tokenIdOpt = Some(k._2))
    }
  }

  def genSelfSpendingTxs(qty: Int): Seq[EncryTransaction] = {

    val pks = genPrivKeys(qty)
    val timestamp = System.currentTimeMillis()
    pks.foldLeft(Seq[EncryTransaction]()) { (seq, key) =>
      val useBoxes =
        if(seq.isEmpty) IndexedSeq(genAssetBox(key.publicImage.address))
        else IndexedSeq(seq.last.newBoxes.head.asInstanceOf[MonetaryBox])
      seq :+ TransactionFactory.defaultPaymentTransactionScratch(key, Props.txFee,
        timestamp, useBoxes, Props.recipientAddr, Props.boxValue)
    }

  }

  def genInvalidPaymentTxs(qty: Int): Seq[EncryTransaction] = {
    val pks = genPrivKeys(qty)
    val timestamp = System.currentTimeMillis()

    pks.map { k =>
      val useBoxes = IndexedSeq(genAssetBox(PublicKey25519(PublicKey @@ Random.randomBytes(32)).address))
      TransactionFactory.defaultPaymentTransactionScratch(k, -100,
        timestamp, useBoxes, Props.recipientAddr, Props.boxValue)
    }
  }

  def genHeader: EncryBlockHeader = {
    val rand = new scala.util.Random
    EncryBlockHeader(
      Random.randomBytes(32)(rand.nextInt(32)),
      PublicKey25519(PublicKey @@ Random.randomBytes()),
      Signature25519(Signature @@ Random.randomBytes(64)),
      ModifierId @@ Random.randomBytes(),
      Digest32 @@ Random.randomBytes(),
      ADDigest @@ Random.randomBytes(33),
      Digest32 @@ Random.randomBytes(),
      rand.nextLong(),
      rand.nextInt(),
      rand.nextLong(),
      Difficulty @@ BigInt(999999999999999L)
    )
  }
}
