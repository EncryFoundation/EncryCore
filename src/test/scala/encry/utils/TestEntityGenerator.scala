package encry.utils


import encry.modifiers.mempool.TransactionFactory
import encry.modifiers.mempool.TransactionFactory._
import encry.settings.{EncryAppSettings, Settings}
import encry.utils.HistoryGenerator.dummyHistory
import encry.utils.TestHelper.Props
import encry.view.history.History
import org.encryfoundation.common.crypto.equihash.EquihashSolution
import org.encryfoundation.common.crypto.{PrivateKey25519, PublicKey25519}
import org.encryfoundation.common.modifiers.history.{Block, Header, Payload}
import org.encryfoundation.common.modifiers.mempool.transaction.EncryAddress.Address
import org.encryfoundation.common.modifiers.mempool.transaction._
import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.modifiers.state.box._
import org.encryfoundation.common.utils.TaggedTypes.{ADKey, Difficulty, Height, ModifierId}
import scorex.crypto.hash.Digest32
import scorex.crypto.signatures.{Curve25519, PrivateKey, PublicKey}
import encry.utils.Utils.randomAddress

import scala.util.Random
import scorex.utils.{Random => ScorexRandom}

object TestEntityGenerator extends Keys with Settings {

  def timestamp: Long = System.currentTimeMillis()

  def genAssetBox(address: Address, amount: Amount = 100000L, tokenIdOpt: Option[ADKey] = None): AssetBox =
    AssetBox(EncryProposition.addressLocked(address), Random.nextLong(), amount, tokenIdOpt)

  def generateDataBox(address: Address, nonce: Long, data: Array[Byte]): DataBox =
    DataBox(EncryProposition.addressLocked(address), nonce, data)

  def generateTokenIssuingBox(address: Address, amount: Amount = 100000L, tokenIdOpt: ADKey): TokenIssuingBox =
    TokenIssuingBox(EncryProposition.addressLocked(address), Random.nextLong(), amount, tokenIdOpt)

  def genTxOutputs(boxes: Traversable[EncryBaseBox]): IndexedSeq[ADKey] =
    boxes.foldLeft(IndexedSeq[ADKey]()) { case (s, box) =>
      s :+ box.id
    }

  def genValidAssetBoxes(secret: PrivateKey25519, amount: Amount, qty: Int): Seq[AssetBox] =
    (0 to qty).foldLeft(IndexedSeq[AssetBox]()) { case (bxs, _) =>
      bxs :+ AssetBox(EncryProposition.pubKeyLocked(secret.publicKeyBytes), Random.nextLong(), amount)
    }

  def genPrivKeys(qty: Int): Seq[PrivateKey25519] = (0 until qty).map { _ =>
    val keys: (PrivateKey, PublicKey) = Curve25519.createKeyPair(ScorexRandom.randomBytes())
    PrivateKey25519(keys._1, keys._2)
  }

  def genValidPaymentTxs(qty: Int): Seq[Transaction] = {
    val keys: Seq[PrivateKey25519] = genPrivKeys(qty)
    val now = System.currentTimeMillis()

    keys.map { k =>
      val useBoxes: IndexedSeq[AssetBox] = IndexedSeq(genAssetBox(k.publicImage.address.address))
      defaultPaymentTransactionScratch(k, Props.txFee,
        now + Random.nextInt(5000), useBoxes, randomAddress, Props.boxValue)
    }
  }

  def genValidPaymentTxsToAddr(qty: Int, address: Address): Seq[Transaction] = {
    val keys: Seq[PrivateKey25519] = genPrivKeys(qty)
    keys.map { k =>
      val useBoxes = IndexedSeq(genAssetBox(address))
      defaultPaymentTransactionScratch(k, Props.txFee,
        Random.nextLong(), useBoxes, address, Props.boxValue)
    }
  }

  def genValidPaymentTxToAddrWithSpentBoxes(boxes: IndexedSeq[AssetBox], address: Address): Transaction = {
    val key: PrivateKey25519 = genPrivKeys(1).head
    defaultPaymentTransactionScratch(key, Props.txFee,
      Random.nextLong(), boxes, address, Props.boxValue)
  }

  def genValidPaymentTxsToAddrWithDiffTokens(qty: Int, address: Address): Seq[Transaction] = {
    val keys: Seq[PrivateKey25519] = genPrivKeys(qty)
    val tokens: Seq[ADKey] = (0 until qty).foldLeft(Seq[ADKey]()) {
      case (seq, _) => seq :+ (ADKey @@ ScorexRandom.randomBytes())
    }
    val pksZipTokens: Seq[(PrivateKey25519, ADKey)] = keys.zip(tokens)
    val timestamp: Amount = System.currentTimeMillis()
    pksZipTokens.map { k =>
      val useBoxes = IndexedSeq(genAssetBox(address, tokenIdOpt = Some(k._2)))
      defaultPaymentTransactionScratch(k._1, Props.txFee,
        timestamp, useBoxes, address, Props.boxValue, tokenIdOpt = Some(k._2))
    }
  }

  def genSelfSpendingTxs(qty: Int): Seq[Transaction] = {
    val keys: Seq[PrivateKey25519] = genPrivKeys(qty)
    val timestamp: Amount = System.currentTimeMillis()
    keys.foldLeft(Seq[Transaction]()) { (seq, key) =>
      val useBoxes: IndexedSeq[MonetaryBox] = if (seq.isEmpty) IndexedSeq(genAssetBox(key.publicImage.address.address))
      else seq.last.newBoxes.map(_.asInstanceOf[MonetaryBox]).toIndexedSeq
      seq :+ defaultPaymentTransactionScratch(key, Props.txFee,
        timestamp, useBoxes, randomAddress, Props.boxValue)
    }
  }

  def genInvalidPaymentTxs(qty: Int): Seq[Transaction] = {
    val timestamp: Amount = System.currentTimeMillis()
    genPrivKeys(qty).map { key =>
      val useBoxes: IndexedSeq[AssetBox] =
        IndexedSeq(genAssetBox(PublicKey25519(PublicKey @@ ScorexRandom.randomBytes(32)).address.address))
      defaultPaymentTransactionScratch(key, -100, timestamp, useBoxes, randomAddress, Props.boxValue)
    }
  }

  def genHeader: Header = {
    val random = new scala.util.Random
    Header(
      1.toByte,
      ModifierId @@ ScorexRandom.randomBytes(),
      Digest32 @@ ScorexRandom.randomBytes(),
      Math.abs(Random.nextLong()),
      Math.abs(Random.nextInt(10000)),
      Random.nextLong(),
      settings.constants.InitialDifficulty,
      EquihashSolution(Seq(1, 3))
    )
  }

  def genHeaderAtHeight(height: Int = 0, transactionsRoot: Digest32 = Digest32 @@ ScorexRandom.randomBytes()): Header = {
    val random = new scala.util.Random
    Header(
      1.toByte,
      ModifierId @@ ScorexRandom.randomBytes(),
      transactionsRoot,
      Math.abs(Random.nextLong()),
      height,
      Random.nextLong(),
      settings.constants.InitialDifficulty,
      EquihashSolution(Seq(1, 3))
    )
  }

  def generatePaymentTransactions(boxes: IndexedSeq[AssetBox],
                                  numberOfInputs: Int,
                                  numberOfOutputs: Int): Vector[Transaction] =
    (0 until boxes.size / numberOfInputs).foldLeft(boxes, Vector.empty[Transaction]) {
      case ((boxesLocal, transactions), _) =>
        val tx: Transaction = paymentTransactionWithMultipleOutputs(
          privKey,
          fee = 111,
          timestamp = 11L,
          useBoxes = boxesLocal.take(numberOfInputs),
          recipient = randomAddress,
          amount = 10000,
          numOfOutputs = numberOfOutputs
        )
        (boxesLocal.drop(numberOfInputs), transactions :+ tx)
    }._2

  def generateDataTransactions(boxes: IndexedSeq[AssetBox],
                               numberOfInputs: Int,
                               numberOfOutputs: Int,
                               bytesQty: Int): Vector[Transaction] =
    (0 until boxes.size / numberOfInputs).foldLeft(boxes, Vector.empty[Transaction]) {
      case ((boxesLocal, transactions), _) =>
        val tx: Transaction = dataTransactionScratch(
          privKey,
          fee = 111,
          timestamp = 11L,
          useOutputs = boxesLocal.take(numberOfInputs),
          data = ScorexRandom.randomBytes(bytesQty),
          amount = 200L,
          numOfOutputs = numberOfOutputs
        )
        (boxesLocal.drop(numberOfInputs), tx +: transactions)
    }._2

  def generateAssetTransactions(boxes: IndexedSeq[AssetBox],
                                numberOfInputs: Int,
                                numberOfOutputs: Int): Vector[Transaction] =
    (0 until boxes.size / numberOfInputs).foldLeft(boxes, Vector.empty[Transaction]) {
      case ((boxesLocal, transactions), _) =>
        val tx: Transaction = assetIssuingTransactionScratch(
          privKey,
          fee = 111,
          timestamp = 11L,
          useOutputs = boxesLocal.take(numberOfInputs),
          amount = 200L,
          numOfOutputs = numberOfOutputs
        )
        (boxesLocal.drop(numberOfInputs), tx +: transactions)
    }._2

  private val genHelper = TestHelper

  lazy val paymentTransactionValid: Transaction = {
    val fee: Amount = genHelper.Props.txFee
    val useBoxes: IndexedSeq[AssetBox] = IndexedSeq(genHelper.genAssetBox(publicKey.address.address),
      genHelper.genAssetBox(publicKey.address.address))

    TransactionFactory.defaultPaymentTransactionScratch(privKey, fee, timestamp, useBoxes,
      publicKey.address.address, genHelper.Props.txAmount)
  }

  def paymentTransactionDynamic: Transaction = {
    val fee = genHelper.Props.txFee
    val useBoxes = (0 to 5).map(_ => {
      AssetBox(
        EncryProposition.pubKeyLocked(publicKey.pubKeyBytes),
        Random.nextLong(),
        999L
      )
    })

    TransactionFactory.defaultPaymentTransactionScratch(privKey, fee, Random.nextLong(), useBoxes,
      publicKey.address.address, genHelper.Props.txAmount)
  }

  def coinbaseTransaction: Transaction = {
    TransactionFactory.coinbaseTransactionScratch(publicKey, timestamp, 10L, 0, Height @@ 100)
  }

  def generateNextBlock(history: History,
                        difficultyDiff: BigInt = 0,
                        prevId: Option[ModifierId] = None,
                        txsQty: Int = 100,
                        additionalDifficulty: BigInt = 0): Block = {
    val previousHeaderId: ModifierId =
      prevId.getOrElse(history.getBestHeader.map(_.id).getOrElse(Header.GenesisParentId))
    val requiredDifficulty: Difficulty = history.getBestHeader.map(parent =>
      history.requiredDifficultyAfter(parent).getOrElse(Difficulty @@ BigInt(0)))
      .getOrElse(history.settings.constants.InitialDifficulty)
    val txs = (if (txsQty != 0) genValidPaymentTxs(Random.nextInt(txsQty)) else Seq.empty) ++
      Seq(coinbaseTransaction)
    val header = genHeader.copy(
      parentId = previousHeaderId,
      height = history.getBestHeaderHeight + 1,
      difficulty = Difficulty @@ (requiredDifficulty + difficultyDiff + additionalDifficulty),
      transactionsRoot = Payload.rootHash(txs.map(_.id))
    )

    Block(header, Payload(header.id, txs))
  }

  def generateForkOn(qty: Int,
                     addDifficulty: BigInt = 0,
                     from: Int,
                     to: Int,
                     settings: EncryAppSettings): (List[Block], List[Block]) = {
    val history: History = dummyHistory(settings)
    val forkInterval = (from until to).toList
    (0 until qty).foldLeft((List(history), List.empty[Block] -> List.empty[Block])) {
      case ((histories, blocks), blockHeight) =>
        if (forkInterval.contains(blockHeight)) {
          if (histories.length == 1) {
            val secondHistory = dummyHistory(settings)
            blocks._1.foldLeft(secondHistory) {
              case (prevHistory, blockToApply) =>
                prevHistory.append(blockToApply.header)
                prevHistory.append(blockToApply.payload)
                prevHistory.reportModifierIsValid(blockToApply)
                prevHistory
            }
            val nextBlockInFirstChain = generateNextBlock(histories.head)
            val nextBlockInSecondChain = generateNextBlock(secondHistory, additionalDifficulty = addDifficulty)
            histories.head.append(nextBlockInFirstChain.header)
            histories.head.append(nextBlockInFirstChain.payload)
            val a = histories.head.reportModifierIsValid(nextBlockInFirstChain)
            secondHistory.append(nextBlockInSecondChain.header)
            secondHistory.append(nextBlockInSecondChain.payload)
            val b = secondHistory.reportModifierIsValid(nextBlockInSecondChain)
            (List(a, b), (blocks._1 :+ nextBlockInFirstChain) -> List(nextBlockInSecondChain))
          } else {
            val nextBlockInFirstChain = generateNextBlock(histories.head)
            val nextBlockInSecondChain = generateNextBlock(histories.last, additionalDifficulty = addDifficulty)
            histories.head.append(nextBlockInFirstChain.header)
            histories.head.append(nextBlockInFirstChain.payload)
            val a = histories.head.reportModifierIsValid(nextBlockInFirstChain)
            histories.last.append(nextBlockInSecondChain.header)
            histories.last.append(nextBlockInSecondChain.payload)
            val b = histories.last.reportModifierIsValid(nextBlockInSecondChain)
            (List(a, b), (blocks._1 :+ nextBlockInFirstChain) -> (blocks._2 :+ nextBlockInSecondChain))
          }
        } else {
          val block: Block = generateNextBlock(histories.head)
          histories.head.append(block.header)
          histories.head.append(block.payload)
          val a = histories.head.reportModifierIsValid(block)
          (List(a), (blocks._1 :+ block) -> blocks._2)
        }
    }._2
  }

}
