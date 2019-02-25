package encry.modifiers.history

import BlockProto.BlockProtoMessage
import BoxesProto.BoxProtoMessage
import HeaderProto.{EquihashSolutionMessage, HeaderProtoMessage}
import PayloadProto.PayloadProtoMessage
import TransactionProto.TransactionProtoMessage
import encry.crypto.equihash.{EquihashSolution, EquihashSolutionsSerializer}
import encry.modifiers.InstanceFactory
import encry.modifiers.mempool.{Transaction, TransactionSerializer}
import encry.modifiers.state.box.{AssetBox, DataBox, EncryBaseBox, TokenIssuingBox}
import encry.settings.EncryAppSettings
import encry.utils.EncryGenerator
import encry.view.history.EncryHistory
import org.encryfoundation.common.Algos
import org.encryfoundation.common.transaction.Pay2PubKeyAddress
import org.encryfoundation.common.utils.TaggedTypes.ADKey
import org.scalatest.{FunSuite, Matchers, PropSpec}
import scorex.crypto.signatures.PublicKey
import scorex.utils.Random

class ModifiersProtoTest extends PropSpec with Matchers with InstanceFactory {

  property("EquihashSolution toProto and fromProto test") {
    val ehs: EquihashSolution = genHeader.equihashSolution
    val ehsToProto: EquihashSolutionMessage = EquihashSolution.toProto(ehs)
    val ehsFromProto: EquihashSolution = EquihashSolution.fromProto(ehsToProto)
    println(s"EHS.toProto = ${ehsToProto.toByteArray.length} vs EHS.bytes = ${ehs.bytes.length}")
    ehs shouldEqual ehsFromProto
  }

  property("Header toProto && fromProto test") {
    val header: Header = genHeader
    val toProtoHeader: HeaderProtoMessage = HeaderSerializer.toProto(header)
    val fromProtoHeader: Header = HeaderSerializer.fromProto(toProtoHeader)
    println(s"header.toProto size = ${toProtoHeader.toByteArray.length} vs header.bytes = ${header.bytes.length}")
    header shouldEqual fromProtoHeader
  }

  property("Transactions toProto && fromProto test") {
    val simplePaymentTx: Transaction = genValidPaymentTxs(1).head
    val simplePaymentTxToProto: TransactionProtoMessage = TransactionSerializer.toProto(simplePaymentTx)
    val simplePaymentTxFromProto: Transaction = TransactionSerializer.fromProto(simplePaymentTxToProto)
    println(s"simplePaymentTx.toProto size = ${simplePaymentTxToProto.toByteArray.length} " +
      s"vs simplePaymentTx.bytes = ${simplePaymentTx.bytes.length}")
    simplePaymentTx shouldEqual simplePaymentTxFromProto

    val simplePaymentTxWithAdd: Transaction =
      genValidPaymentTxsToAddr(1, Pay2PubKeyAddress(PublicKey @@ Random.randomBytes()).address).head
    val simplePaymentTxWithAddToProto: TransactionProtoMessage = TransactionSerializer.toProto(simplePaymentTxWithAdd)
    val simplePaymentTxWithAddFromProto: Transaction = TransactionSerializer.fromProto(simplePaymentTxWithAddToProto)
    println(s"simplePaymentTxWithAdd.toProto size = ${simplePaymentTxWithAddToProto.toByteArray.length} " +
      s"vs simplePaymentTxWithAdd.bytes = ${simplePaymentTxWithAdd.bytes.length}")
    simplePaymentTxWithAdd shouldEqual simplePaymentTxWithAddFromProto

    val simplePaymentTxWithToken: Transaction =
      genValidPaymentTxsToAddrWithDiffTokens(1, Pay2PubKeyAddress(PublicKey @@ Random.randomBytes()).address).head
    val simplePaymentTxWithTokenToProto: TransactionProtoMessage = TransactionSerializer.toProto(simplePaymentTxWithToken)
    val simplePaymentTxWithTokenFromProto: Transaction = TransactionSerializer.fromProto(simplePaymentTxWithTokenToProto)
    println(s"simplePaymentTxWithToken.toProto size = ${simplePaymentTxWithTokenToProto.toByteArray.length} " +
      s"vs simplePaymentTxWithToken.bytes = ${simplePaymentTxWithToken.bytes.length}")
    simplePaymentTxWithToken shouldEqual simplePaymentTxWithTokenFromProto

    val box: IndexedSeq[AssetBox] =
      IndexedSeq(genAssetBox(Pay2PubKeyAddress(PublicKey @@ Random.randomBytes()).address, 10000000L))

    val simpleDataTx: Transaction =
      generateDataTransactions(box, 1, 1, 999).head
    val simpleDataTxToProto: TransactionProtoMessage = TransactionSerializer.toProto(simpleDataTx)
    val simpleDataTxFromProto: Transaction = TransactionSerializer.fromProto(simpleDataTxToProto)
    println(s"simpleDataTx.toProto size = ${simpleDataTxToProto.toByteArray.length} " +
      s"vs simpleDataTx.bytes = ${simpleDataTx.bytes.length}")
    simpleDataTx shouldEqual simpleDataTxFromProto

    val simpleAssetIssuingTransaction: Transaction =
      generateAssetTransactions(box, 1, 1).head
    val simpleAssetIssuingTransactionToProto: TransactionProtoMessage = TransactionSerializer.toProto(simpleAssetIssuingTransaction)
    val simpleAssetIssuingTransactionFromProto: Transaction = TransactionSerializer.fromProto(simpleAssetIssuingTransactionToProto)
    println(s"simpleAssetIssuingTransaction.toProto size = ${simpleAssetIssuingTransactionToProto.toByteArray.length} " +
      s"vs simpleAssetIssuingTransaction.bytes = ${simpleAssetIssuingTransaction.bytes.length}")
    simpleAssetIssuingTransaction shouldEqual simpleAssetIssuingTransactionFromProto
  }

  property("Payload toProto && FromProto test") {
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
    val payloadToProto: PayloadProtoMessage = PayloadSerializer.toProto(payload)
    val payloadFromProto: Payload = PayloadSerializer.fromProto(payloadToProto)
    println(s"payload.toProto size = ${payloadToProto.toByteArray.length} " +
      s"vs payload.bytes = ${payload.bytes.length}")
    payload shouldEqual payloadFromProto
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
    val blockToProto: BlockProtoMessage = BlockSerializer.toProto(block)
    val blockFromProto: Block = BlockSerializer.fromProto(blockToProto)
    println(s"block.toProto size = ${blockToProto.toByteArray.length} " +
      s"vs block.bytes = ${block.bytes.length}")
    block shouldEqual blockFromProto
  }

  property("Boxes toProto && fromProto test") {
    val assetBox: AssetBox =
      genAssetBox(Pay2PubKeyAddress(PublicKey @@ Random.randomBytes()).address, 1000000L)
    val assetBoxToProto: BoxProtoMessage = assetBox.toProto(assetBox)
    val assetBoxFromProto: AssetBox = assetBox.fromProto(assetBoxToProto) match { case s: AssetBox => s }
    println(s"assetBox.toProto size = ${assetBoxToProto.toByteArray.length} vs assetBox.bytes = ${assetBox.bytes.length}")
    assetBox.proposition.contractHash shouldEqual assetBoxFromProto.proposition.contractHash
    assetBox.nonce shouldEqual assetBoxFromProto.nonce
    assetBox.amount shouldEqual assetBoxFromProto.amount
    assetBox.tokenIdOpt shouldEqual assetBoxFromProto.tokenIdOpt

    val dataBox: DataBox =
      generateDataBox(Pay2PubKeyAddress(PublicKey @@ Random.randomBytes()).address, 100000L, Random.randomBytes())
    val dataBoxToProto: BoxProtoMessage = dataBox.toProto(dataBox)
    val dataBoxFromProto: DataBox = dataBox.fromProto(dataBoxToProto) match { case s: DataBox => s }
    println(s"dataBox.toProto size = ${dataBoxToProto.toByteArray.length} vs dataBox.bytes = ${dataBox.bytes.length}")
    dataBox.proposition.contractHash shouldEqual dataBoxFromProto.proposition.contractHash
    dataBox.nonce shouldEqual dataBoxFromProto.nonce
    dataBox.data shouldEqual dataBoxFromProto.data

    val tokenIssuingBox =
      generateTokenIssuingBox(Pay2PubKeyAddress(PublicKey @@ Random.randomBytes()).address,
        1000000L, ADKey @@ Random.randomBytes())
    val tokenIssuingBoxToProto: BoxProtoMessage = tokenIssuingBox.toProto(tokenIssuingBox)
    val tokenIssuingBoxFromProto: TokenIssuingBox = tokenIssuingBox.fromProto(tokenIssuingBoxToProto) match { case s: TokenIssuingBox => s }
    println(s"tokenIssuingBox.toProto size = ${tokenIssuingBoxToProto.toByteArray.length} " +
      s"vs tokenIssuingBox.bytes = ${tokenIssuingBox.bytes.length}")
    tokenIssuingBox.proposition.contractHash shouldEqual tokenIssuingBoxFromProto.proposition.contractHash
    tokenIssuingBox.nonce shouldEqual tokenIssuingBoxFromProto.nonce
    tokenIssuingBox.amount shouldEqual tokenIssuingBoxFromProto.amount
    tokenIssuingBox.tokenId shouldEqual tokenIssuingBoxFromProto.tokenId
  }
}