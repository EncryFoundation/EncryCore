package encry.utils

import encry.modifiers.mempool.TransactionFactory
import encry.settings.Settings
import encry.utils.TestHelper.Props
import org.encryfoundation.common.crypto.equihash.EquihashSolution
import org.encryfoundation.common.crypto.{PrivateKey25519, PublicKey25519, Signature25519}
import org.encryfoundation.common.modifiers.history.Header
import org.encryfoundation.common.modifiers.mempool.directive._
import org.encryfoundation.common.modifiers.mempool.transaction.EncryAddress.Address
import org.encryfoundation.common.modifiers.mempool.transaction._
import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.modifiers.state.box._
import org.encryfoundation.common.utils.TaggedTypes.{ADKey, ModifierId}
import org.encryfoundation.prismlang.core.wrapped.BoxedValue
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.crypto.signatures.{Curve25519, PrivateKey, PublicKey}
import scorex.utils.Random

import scala.util.{Random => ScRand}

trait EncryGenerator extends Settings {

  Box

  val mnemonicKey: String = "index another island accuse valid aerobic little absurd bunker keep insect scissors"
  val privKey: PrivateKey25519 = createPrivKey(Some(mnemonicKey))


  def createPrivKey(seed: Option[String]): PrivateKey25519 = {
    val (privateKey: PrivateKey, publicKey: PublicKey) = Curve25519.createKeyPair(
      Blake2b256.hash(
        seed.map {
          Mnemonic.seedFromMnemonic(_)
        }
          .getOrElse {
            val phrase: String = Mnemonic.entropyToMnemonicCode(scorex.utils.Random.randomBytes(16))
            Mnemonic.seedFromMnemonic(phrase)
          })
    )
    PrivateKey25519(privateKey, publicKey)
  }

  def protocolToBytes(protocol: String): Array[Byte] = protocol.split("\\.").map(elem => elem.toByte)

  def timestamp: Long = System.currentTimeMillis()

  def randomAddress: Address = Pay2PubKeyAddress(PublicKey @@ Random.randomBytes()).address

  def genAssetBox(address: Address, amount: Amount = 100000L, tokenIdOpt: Option[ADKey] = None, nonce: Long = 0L): AssetBox =
    AssetBox(EncryProposition.addressLocked(address), nonce, amount, tokenIdOpt)

  def generateDataBox(address: Address, nonce: Long, data: Array[Byte]): DataBox =
    DataBox(EncryProposition.addressLocked(address), nonce, data)

  def generateTokenIssuingBox(address: Address, amount: Amount = 100000L, tokenIdOpt: ADKey): TokenIssuingBox =
    TokenIssuingBox(EncryProposition.addressLocked(address), ScRand.nextLong(), amount, tokenIdOpt)

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

  def genSelfSpendingTxs(qty: Int): Seq[Transaction] = {
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
      Math.abs(random.nextLong()),
      Math.abs(random.nextInt(10000)),
      random.nextLong(),
      settings.constants.InitialDifficulty,
      EquihashSolution(Seq(1, 3)),
      Random.randomBytes()
    )
  }

  def genHeaderAtHeight(height: Int = 0, transactionsRoot: Digest32 = Digest32 @@ Random.randomBytes()): Header = {
    val random = new scala.util.Random
    Header(
      1.toByte,
      ModifierId @@ Random.randomBytes(),
      transactionsRoot,
      Math.abs(random.nextLong()),
      height,
      random.nextLong(),
      settings.constants.InitialDifficulty,
      EquihashSolution(Seq(1, 3)),
      Random.randomBytes()
    )
  }

  def generatePaymentTransactions(privKey: PrivateKey25519,
                                   boxes: IndexedSeq[AssetBox],
                                  numberOfInputs: Int,
                                  numberOfOutputs: Int): Vector[Transaction] =
    (0 until boxes.size / numberOfInputs).foldLeft(boxes, Vector.empty[Transaction]) {
      case ((boxesLocal, transactions), _) =>
        val tx: Transaction = defaultPaymentTransactionScratch(
          privKey,
          fee = 11,
          timestamp = 11L,
          useBoxes = boxesLocal.take(numberOfInputs),
          recipient = randomAddress,
          amount = 1,
          numOfOutputs = numberOfOutputs
        )
        (boxesLocal.drop(numberOfInputs), transactions :+ tx)
    }._2

  def generatePaymentTransactions(boxes: IndexedSeq[AssetBox],
                                  numberOfInputs: Int,
                                  numberOfOutputs: Int): Vector[Transaction] =
    (0 until boxes.size / numberOfInputs).foldLeft(boxes, Vector.empty[Transaction]) {
      case ((boxesLocal, transactions), _) =>
        val tx: Transaction = defaultPaymentTransactionScratch(
          privKey,
          fee = 0,
          timestamp = 11L,
          useBoxes = boxesLocal.take(numberOfInputs),
          recipient = randomAddress,
          amount = 1,
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
          data = Random.randomBytes(bytesQty),
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

  def defaultPaymentTransactionScratch(privKey: PrivateKey25519,
                                       fee: Amount,
                                       timestamp: Long,
                                       useBoxes: IndexedSeq[MonetaryBox],
                                       recipient: Address,
                                       amount: Amount,
                                       tokenIdOpt: Option[ADKey] = None,
                                       numOfOutputs: Int = 5): Transaction = {

    val pubKey: PublicKey25519 = privKey.publicImage

    val uInputs: IndexedSeq[Input] = useBoxes
      .map(bx => Input.unsigned(bx.id, Right(PubKeyLockedContract(pubKey.pubKeyBytes))))
      .toIndexedSeq

    val change: Amount = useBoxes.map(_.amount).sum - (amount + fee)

    val directives: IndexedSeq[TransferDirective] =
      if (change > 0) TransferDirective(recipient, amount, tokenIdOpt) +: (0 until numOfOutputs).map(_ =>
        TransferDirective(pubKey.address.address, change / numOfOutputs, tokenIdOpt))
      else IndexedSeq(TransferDirective(recipient, amount, tokenIdOpt))

    val uTransaction: UnsignedTransaction = UnsignedTransaction(fee, timestamp, uInputs, directives)

    val signature: Signature25519 = privKey.sign(uTransaction.messageToSign)

    uTransaction.toSigned(IndexedSeq.empty, Some(Proof(BoxedValue.Signature25519Value(signature.bytes.toList))))
  }

  def dataTransactionScratch(privKey: PrivateKey25519,
                             fee: Long,
                             timestamp: Long,
                             useOutputs: IndexedSeq[MonetaryBox],
                             amount: Long,
                             data: Array[Byte],
                             numOfOutputs: Int = 5): Transaction = {

    val pubKey: PublicKey25519 = privKey.publicImage

    val uInputs: IndexedSeq[Input] = useOutputs
      .map(bx => Input.unsigned(bx.id, Right(PubKeyLockedContract(pubKey.pubKeyBytes))))
      .toIndexedSeq

    val change: Amount = useOutputs.map(_.amount).sum - (amount + fee)

    val directives: IndexedSeq[DataDirective] =
      (0 until numOfOutputs).foldLeft(IndexedSeq.empty[DataDirective]) { case (directivesAll, _) =>
        directivesAll :+ DataDirective(PubKeyLockedContract(privKey.publicImage.pubKeyBytes).contract.hash, data)
      }

    val newDirectives: IndexedSeq[Directive] =
      if (change > 0) TransferDirective(pubKey.address.address, amount, None) +: (0 until numOfOutputs).map(_ =>
        TransferDirective(pubKey.address.address, change / numOfOutputs, None)) ++: directives
      else directives

    val uTransaction: UnsignedTransaction = UnsignedTransaction(fee, timestamp, uInputs, newDirectives)

    val signature: Signature25519 = privKey.sign(uTransaction.messageToSign)

    uTransaction.toSigned(IndexedSeq.empty, Some(Proof(BoxedValue.Signature25519Value(signature.bytes.toList))))
  }

  def assetIssuingTransactionScratch(privKey: PrivateKey25519,
                                     fee: Long,
                                     timestamp: Long,
                                     useOutputs: IndexedSeq[MonetaryBox],
                                     amount: Long,
                                     numOfOutputs: Int = 5): Transaction = {
    val directives: IndexedSeq[AssetIssuingDirective] =
      (0 until numOfOutputs).foldLeft(IndexedSeq.empty[AssetIssuingDirective]) { case (directivesAll, _) =>
        directivesAll :+ AssetIssuingDirective(PubKeyLockedContract(privKey.publicImage.pubKeyBytes).contract.hash, amount)
      }

    val pubKey: PublicKey25519 = privKey.publicImage

    val uInputs: IndexedSeq[Input] = useOutputs
      .map(bx => Input.unsigned(bx.id, Right(PubKeyLockedContract(pubKey.pubKeyBytes))))
      .toIndexedSeq

    val change: Amount = useOutputs.map(_.amount).sum - (amount + fee)

    val newDirectives: IndexedSeq[Directive] =
      if (change > 0) TransferDirective(pubKey.address.address, amount, None) +: (0 until numOfOutputs).map(_ =>
        TransferDirective(pubKey.address.address, change / numOfOutputs, None)) ++: directives
      else directives

    val uTransaction: UnsignedTransaction = UnsignedTransaction(fee, timestamp, uInputs, newDirectives)

    val signature: Signature25519 = privKey.sign(uTransaction.messageToSign)

    uTransaction.toSigned(IndexedSeq.empty, Some(Proof(BoxedValue.Signature25519Value(signature.bytes.toList))))
  }

  def universalTransactionScratch(privKey: PrivateKey25519,
                                  fee: Long,
                                  timestamp: Long,
                                  useOutputs: IndexedSeq[MonetaryBox],
                                  amount: Long,
                                  numOfOutputs: Int = 5): Transaction = {
    val directives: IndexedSeq[Directive] = IndexedSeq(
      AssetIssuingDirective(PubKeyLockedContract(privKey.publicImage.pubKeyBytes).contract.hash, amount),
      DataDirective(PubKeyLockedContract(privKey.publicImage.pubKeyBytes).contract.hash, Random.randomBytes()),
      ScriptedAssetDirective(PubKeyLockedContract(privKey.publicImage.pubKeyBytes).contract.hash, 10L,
        Option(ADKey @@ Random.randomBytes())),
      ScriptedAssetDirective(PubKeyLockedContract(privKey.publicImage.pubKeyBytes).contract.hash, 10L,
        Option.empty[ADKey]),
      TransferDirective(privKey.publicImage.address.address, 10L, Option(ADKey @@ Random.randomBytes())),
      TransferDirective(privKey.publicImage.address.address, 10L, Option.empty[ADKey])
    )

    val pubKey: PublicKey25519 = privKey.publicImage

    val uInputs: IndexedSeq[Input] = useOutputs
      .map(bx => Input.unsigned(bx.id, Right(PubKeyLockedContract(pubKey.pubKeyBytes))))
      .toIndexedSeq

    val change: Amount = useOutputs.map(_.amount).sum - (amount + fee)

    val newDirectives: IndexedSeq[Directive] =
      if (change > 0) TransferDirective(pubKey.address.address, amount, None) +: (0 until numOfOutputs).map(_ =>
        TransferDirective(pubKey.address.address, change / numOfOutputs, None)) ++: directives
      else directives

    val uTransaction: UnsignedTransaction = UnsignedTransaction(fee, timestamp, uInputs, newDirectives)

    val signature: Signature25519 = privKey.sign(uTransaction.messageToSign)

    uTransaction.toSigned(IndexedSeq.empty, Some(Proof(BoxedValue.Signature25519Value(signature.bytes.toList))))
  }
}
