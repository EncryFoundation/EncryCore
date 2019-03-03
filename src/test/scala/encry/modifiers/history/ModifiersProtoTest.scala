package encry.modifiers.history

import BlockProto.BlockProtoMessage
import BoxesProto.BoxProtoMessage
import HeaderProto.HeaderProtoMessage
import HeaderProto.HeaderProtoMessage.EquihashSolutionMessage
import PayloadProto.PayloadProtoMessage
import TransactionProto.TransactionProtoMessage
import encry.crypto.equihash.{EquihashSolution, EquihashSolutionsSerializer}
import encry.modifiers.InstanceFactory
import encry.modifiers.mempool.directive._
import encry.modifiers.mempool.{Transaction, TransactionProtoSerializer, TransactionSerializer}
import encry.modifiers.state.box.{AssetBox, DataBox, EncryBaseBox, TokenIssuingBox}
import encry.settings.EncryAppSettings
import encry.utils.EncryGenerator
import encry.view.history.EncryHistory
import org.encryfoundation.common.Algos
import org.encryfoundation.common.transaction.{Pay2PubKeyAddress, PubKeyLockedContract}
import org.encryfoundation.common.utils.TaggedTypes.ADKey
import org.scalatest.{FunSuite, Matchers, PropSpec}
import scorex.crypto.signatures.PublicKey
import scorex.utils.Random

import scala.util.Try

class ModifiersProtoTest extends PropSpec with Matchers with InstanceFactory {

  //todo add tests for merkel root, sing

  property("AssetIssuingDirective should be serialized correctly") {
    val assetIssuingDirective: AssetIssuingDirective =
      AssetIssuingDirective(PubKeyLockedContract(privKey.publicImage.pubKeyBytes).contract.hash, 1000L)
    val assetIssuingDirectiveToProto: TransactionProtoMessage.DirectiveProtoMessage = assetIssuingDirective.toDirectiveProto
    val assetIssuingDirectiveFromProto: Option[AssetIssuingDirective] =
      AssetIssuingDirectiveProtoSerializer.fromProto(assetIssuingDirectiveToProto)
    assetIssuingDirectiveFromProto.isDefined shouldBe true
    assetIssuingDirective.contractHash.sameElements(assetIssuingDirectiveFromProto.get.contractHash) shouldBe true
    assetIssuingDirective.amount shouldEqual assetIssuingDirectiveFromProto.get.amount
  }

  property("DataDirective should be serialized correctly") {
    val dataDirective: DataDirective =
      DataDirective(PubKeyLockedContract(privKey.publicImage.pubKeyBytes).contract.hash, Random.randomBytes())
    val dataDirectiveToProto: TransactionProtoMessage.DirectiveProtoMessage = dataDirective.toDirectiveProto
    val dataDirectiveFromProto = DataDirectiveProtoSerializer.fromProto(dataDirectiveToProto)
    dataDirectiveFromProto.isDefined shouldBe true
    dataDirective.contractHash.sameElements(dataDirectiveFromProto.get.contractHash) shouldBe true
    dataDirective.data.sameElements(dataDirectiveFromProto.get.data)
  }

  property("ScriptedAssetDirective should be serialized correctly") {
    val scriptedAssetDirectiveWithTokenId: ScriptedAssetDirective =
      ScriptedAssetDirective(PubKeyLockedContract(privKey.publicImage.pubKeyBytes).contract.hash, 10L,
        Option(ADKey @@ Random.randomBytes()))
    val scriptedAssetDirectiveWithTokenIdToProto: TransactionProtoMessage.DirectiveProtoMessage =
      scriptedAssetDirectiveWithTokenId.toDirectiveProto
    val scriptedAssetDirectiveWithTokenIdFromProto: Option[ScriptedAssetDirective] =
      ScriptedAssetDirectiveProtoSerializer.fromProto(scriptedAssetDirectiveWithTokenIdToProto)
    scriptedAssetDirectiveWithTokenIdFromProto.isDefined shouldBe true
    scriptedAssetDirectiveWithTokenId.contractHash.sameElements(scriptedAssetDirectiveWithTokenIdFromProto.get.contractHash) shouldBe true
    scriptedAssetDirectiveWithTokenId.amount shouldEqual scriptedAssetDirectiveWithTokenIdFromProto.get.amount
    scriptedAssetDirectiveWithTokenId.tokenIdOpt.get.sameElements(scriptedAssetDirectiveWithTokenIdFromProto.get.tokenIdOpt.get)

    val scriptedAssetDirectiveWithoutTokenId: ScriptedAssetDirective =
      ScriptedAssetDirective(PubKeyLockedContract(privKey.publicImage.pubKeyBytes).contract.hash, 10L,
        Option.empty[ADKey])
    val scriptedAssetDirectiveWithoutTokenIdToProto: TransactionProtoMessage.DirectiveProtoMessage =
      scriptedAssetDirectiveWithoutTokenId.toDirectiveProto
    val scriptedAssetDirectiveWithoutTokenIdFromProto: Option[ScriptedAssetDirective] =
      ScriptedAssetDirectiveProtoSerializer.fromProto(scriptedAssetDirectiveWithoutTokenIdToProto)
    scriptedAssetDirectiveWithoutTokenIdFromProto.isDefined shouldBe true
    scriptedAssetDirectiveWithoutTokenId.contractHash.sameElements(scriptedAssetDirectiveWithoutTokenIdFromProto.get.contractHash) shouldBe true
    scriptedAssetDirectiveWithoutTokenId.amount shouldEqual scriptedAssetDirectiveWithoutTokenIdFromProto.get.amount
    scriptedAssetDirectiveWithoutTokenIdFromProto.get.tokenIdOpt shouldBe None
  }

  property("TransferDirective should be serialized correctly") {
    val transferDirectiveWithTokenId: TransferDirective =
      TransferDirective(privKey.publicImage.address.address, 10L, Option(ADKey @@ Random.randomBytes()))
    val transferDirectiveDirectiveWithTokenIdToProto: TransactionProtoMessage.DirectiveProtoMessage =
      transferDirectiveWithTokenId.toDirectiveProto
    val transferDirectiveWithTokenIdFromProto: Option[TransferDirective] =
      TransferDirectiveProtoSerializer.fromProto(transferDirectiveDirectiveWithTokenIdToProto)
    transferDirectiveWithTokenIdFromProto.isDefined shouldBe true
    transferDirectiveWithTokenId.address.sameElements(transferDirectiveWithTokenIdFromProto.get.address) shouldBe true
    transferDirectiveWithTokenId.amount shouldEqual transferDirectiveWithTokenIdFromProto.get.amount
    transferDirectiveWithTokenId.tokenIdOpt.get.sameElements(transferDirectiveWithTokenIdFromProto.get.tokenIdOpt.get)

    val transferDirectiveWithoutTokenId: TransferDirective =
      TransferDirective(privKey.publicImage.address.address, 10L, Option.empty[ADKey])
    val transferDirectiveWithoutTokenIdToProto: TransactionProtoMessage.DirectiveProtoMessage =
      transferDirectiveWithoutTokenId.toDirectiveProto
    val transferDirectiveWithoutTokenIdFromProto: Option[TransferDirective] =
      TransferDirectiveProtoSerializer.fromProto(transferDirectiveWithoutTokenIdToProto)
    transferDirectiveWithoutTokenIdFromProto.isDefined shouldBe true
    transferDirectiveWithoutTokenId.address.sameElements(transferDirectiveWithoutTokenIdFromProto.get.address) shouldBe true
    transferDirectiveWithoutTokenId.amount shouldEqual transferDirectiveWithoutTokenIdFromProto.get.amount
    transferDirectiveWithoutTokenIdFromProto.get.tokenIdOpt shouldBe None
  }

  property("Transaction should be serialized correctly") {
    val boxes: Seq[AssetBox] = (0 to 10).map(_ => genAssetBox(privKey.publicImage.address.address))
    val transaction: Transaction =
      universalTransactionScratch(privKey, 10, 10, boxes.toIndexedSeq, 10, 1)
    val transactionToProto: TransactionProtoMessage = transaction.toTransactionProto
    val transactionFromProto: Try[Transaction] = TransactionProtoSerializer.fromProto(transactionToProto)
    transactionFromProto.isSuccess shouldBe true
    transaction shouldEqual transactionFromProto.get

    val simplePaymentTx: Transaction = genValidPaymentTxs(1).head
    val simplePaymentTxToProto: TransactionProtoMessage = simplePaymentTx.toTransactionProto
    val simplePaymentTxFromProto: Try[Transaction] = TransactionProtoSerializer.fromProto(simplePaymentTxToProto)
    simplePaymentTx shouldEqual simplePaymentTxFromProto.get

    val simplePaymentTxWithAdd: Transaction =
      genValidPaymentTxsToAddr(1, Pay2PubKeyAddress(PublicKey @@ Random.randomBytes()).address).head
    val simplePaymentTxWithAddToProto: TransactionProtoMessage = simplePaymentTxWithAdd.toTransactionProto
    val simplePaymentTxWithAddFromProto: Try[Transaction] = TransactionProtoSerializer.fromProto(simplePaymentTxWithAddToProto)
    simplePaymentTxWithAdd shouldEqual simplePaymentTxWithAddFromProto.get

    val simplePaymentTxWithToken: Transaction =
      genValidPaymentTxsToAddrWithDiffTokens(1, Pay2PubKeyAddress(PublicKey @@ Random.randomBytes()).address).head
    val simplePaymentTxWithTokenToProto: TransactionProtoMessage = simplePaymentTxWithToken.toTransactionProto
    val simplePaymentTxWithTokenFromProto: Try[Transaction] = TransactionProtoSerializer.fromProto(simplePaymentTxWithTokenToProto)
    simplePaymentTxWithToken shouldEqual simplePaymentTxWithTokenFromProto.get

    val box: IndexedSeq[AssetBox] =
      IndexedSeq(genAssetBox(Pay2PubKeyAddress(PublicKey @@ Random.randomBytes()).address, 10000000L))

    val simpleDataTx: Transaction =
      generateDataTransactions(box, 1, 1, 999).head
    val simpleDataTxToProto: TransactionProtoMessage = simpleDataTx.toTransactionProto
    val simpleDataTxFromProto: Try[Transaction] = TransactionProtoSerializer.fromProto(simpleDataTxToProto)
    simpleDataTx shouldEqual simpleDataTxFromProto.get

    val simpleAssetIssuingTransaction: Transaction =
      generateAssetTransactions(box, 1, 1).head
    val simpleAssetIssuingTransactionToProto: TransactionProtoMessage = simpleAssetIssuingTransaction.toTransactionProto
    val simpleAssetIssuingTransactionFromProto: Try[Transaction] = TransactionProtoSerializer.fromProto(simpleAssetIssuingTransactionToProto)
    simpleAssetIssuingTransaction shouldEqual simpleAssetIssuingTransactionFromProto.get
  }

  property("Payload should be serialized correctly") {
    val header: Header = genHeader
    val box1: IndexedSeq[AssetBox] =
      IndexedSeq(genAssetBox(Pay2PubKeyAddress(PublicKey @@ Random.randomBytes()).address, 1000000L))
    val box2: IndexedSeq[AssetBox] =
      IndexedSeq(genAssetBox(Pay2PubKeyAddress(PublicKey @@ Random.randomBytes()).address, 1000000L))
    val box3: IndexedSeq[AssetBox] =
      IndexedSeq(genAssetBox(Pay2PubKeyAddress(PublicKey @@ Random.randomBytes()).address, 1000000L))
    val transactions: Vector[Transaction] =
      generateAssetTransactions(box1, 1, 1) ++
        generateDataTransactions(box2, 1, 1, 999) ++
        generatePaymentTransactions(box3, 1, 1)
    val payload: Payload = Payload(header.id, transactions)
    val payloadToProto: PayloadProtoMessage = payload.toProtoPayload
    val payloadFromProto: Try[Payload] = PayloadProtoSerializer.fromProto(payloadToProto)
    payload shouldEqual payloadFromProto.get
  }

  ///TODO maybe compare all fields!?
  property("EquihashSolution toProto and fromProto test") {
    val ehs: EquihashSolution = genHeader.equihashSolution
    val ehsToProto: EquihashSolutionMessage = EquihashSolution.toProto(ehs)
    val ehsFromProto: EquihashSolution = EquihashSolution.fromProto(ehsToProto)
    ehs shouldEqual ehsFromProto
  }

  ///TODO maybe compare all fields!?
  property("Header toProto && fromProto test") {
    val header: Header = genHeader
    val toProtoHeader: HeaderProtoMessage = header.toHeaderProto
    val fromProtoHeader: Try[Header] = HeaderProtoSerializer.fromProto(toProtoHeader)
    header shouldEqual fromProtoHeader.get
  }

  property("Block toProto && fromProto test") {
    val header: Header = genHeader
    val box1: IndexedSeq[AssetBox] =
      IndexedSeq(genAssetBox(Pay2PubKeyAddress(PublicKey @@ Random.randomBytes()).address, 1000000L))
    val box2: IndexedSeq[AssetBox] =
      IndexedSeq(genAssetBox(Pay2PubKeyAddress(PublicKey @@ Random.randomBytes()).address, 1000000L))
    val box3: IndexedSeq[AssetBox] =
      IndexedSeq(genAssetBox(Pay2PubKeyAddress(PublicKey @@ Random.randomBytes()).address, 1000000L))
    val transactions: Vector[Transaction] =
      generateAssetTransactions(box1, 1, 1) ++
        generateDataTransactions(box2, 1, 1, 999) ++
        generatePaymentTransactions(box3, 1, 1)
    val payload: Payload = Payload(header.id, transactions)
    val block = Block(header, payload, None)
    val blockToProto: BlockProtoMessage = block.toProtoBlock
    val blockFromProto: Block = BlockProtoSerializer.fromProto(blockToProto)
    block shouldEqual blockFromProto
  }
}

//
//  property("Boxes toProto && fromProto test") {
//    val assetBox: AssetBox =
//      genAssetBox(Pay2PubKeyAddress(PublicKey @@ Random.randomBytes()).address, 1000000L)
//    val assetBoxToProto: BoxProtoMessage = assetBox.toProto(assetBox)
//    val assetBoxFromProto: AssetBox = assetBox.fromProto(assetBoxToProto) match {
//      case s: AssetBox => s
//    }
//    println(s"assetBox.toProto size = ${assetBoxToProto.toByteArray.length} vs assetBox.bytes = ${assetBox.bytes.length}")
//    assetBox.proposition.contractHash shouldEqual assetBoxFromProto.proposition.contractHash
//    assetBox.nonce shouldEqual assetBoxFromProto.nonce
//    assetBox.amount shouldEqual assetBoxFromProto.amount
//    assetBox.tokenIdOpt shouldEqual assetBoxFromProto.tokenIdOpt
//
//    val dataBox: DataBox =
//      generateDataBox(Pay2PubKeyAddress(PublicKey @@ Random.randomBytes()).address, 100000L, Random.randomBytes())
//    val dataBoxToProto: BoxProtoMessage = dataBox.toProto(dataBox)
//    val dataBoxFromProto: DataBox = dataBox.fromProto(dataBoxToProto) match {
//      case s: DataBox => s
//    }
//    println(s"dataBox.toProto size = ${dataBoxToProto.toByteArray.length} vs dataBox.bytes = ${dataBox.bytes.length}")
//    dataBox.proposition.contractHash shouldEqual dataBoxFromProto.proposition.contractHash
//    dataBox.nonce shouldEqual dataBoxFromProto.nonce
//    dataBox.data shouldEqual dataBoxFromProto.data
//
//    val tokenIssuingBox =
//      generateTokenIssuingBox(Pay2PubKeyAddress(PublicKey @@ Random.randomBytes()).address,
//        1000000L, ADKey @@ Random.randomBytes())
//    val tokenIssuingBoxToProto: BoxProtoMessage = tokenIssuingBox.toProto(tokenIssuingBox)
//    val tokenIssuingBoxFromProto: TokenIssuingBox = tokenIssuingBox.fromProto(tokenIssuingBoxToProto) match {
//      case s: TokenIssuingBox => s
//    }
//    println(s"tokenIssuingBox.toProto size = ${tokenIssuingBoxToProto.toByteArray.length} " +
//      s"vs tokenIssuingBox.bytes = ${tokenIssuingBox.bytes.length}")
//    tokenIssuingBox.proposition.contractHash shouldEqual tokenIssuingBoxFromProto.proposition.contractHash
//    tokenIssuingBox.nonce shouldEqual tokenIssuingBoxFromProto.nonce
//    tokenIssuingBox.amount shouldEqual tokenIssuingBoxFromProto.amount
//    tokenIssuingBox.tokenId shouldEqual tokenIssuingBoxFromProto.tokenId
//  }
//
//  property("Test modifiers proto size vs bytes") {
//    val settings: EncryAppSettings = EncryAppSettings.read
//
//    val historyWith100Blocks: (EncryHistory, Vector[Block]) =
//      (0 until 100).foldLeft(generateDummyHistory(settings), Vector.empty[Block]) {
//        case (prevHistory, _) =>
//          val block: Block = generateNextBlock(prevHistory._1)
//          (prevHistory._1.append(block.header).get._1.append(block.payload).get._1.reportModifierIsValid(block),
//            prevHistory._2 :+ block)
//      }
//    val a = historyWith100Blocks._2.map(x => BlockSerializer.toProto(x).toByteArray.length).sum
//    val b = historyWith100Blocks._2.map(x => x.bytes.length).sum
//    println(s"Block: toProto - ${a / 1024} kbytes, toBytes - ${b / 1024} kbytes")
//
//    val a1 = historyWith100Blocks._2.map(x => HeaderSerializer.toProto(x.header).toByteArray.length).sum
//    val b1 = historyWith100Blocks._2.map(x => x.header.bytes.length).sum
//    println(s"Header: toProto - ${a1 / 1024} kbytes, toBytes - ${b1 / 1024} kbytes")
//
//    val a2 = historyWith100Blocks._2.map(x => PayloadSerializer.toProto(x.payload).toByteArray.length).sum
//    val b2 = historyWith100Blocks._2.map(x => x.payload.bytes.length).sum
//    println(s"Payload: toProto - ${a2 / 1024} kbytes, toBytes - ${b2 / 1024} kbytes")
//  }
//}