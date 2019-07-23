package encry.it.contracts

import TransactionGenerator.CreateTransaction
import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.EncrySupplyController
import encry.it.configs.Configs
import encry.it.docker.NodesFromDocker
import encry.it.util.KeyHelper._
import org.encryfoundation.common.crypto.{PrivateKey25519, PublicKey25519}
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.modifiers.mempool.transaction.EncryAddress.Address
import org.encryfoundation.common.modifiers.mempool.transaction.{Proof, PubKeyLockedContract, Transaction}
import org.encryfoundation.common.modifiers.state.box.TokenIssuingBox.TokenId
import org.encryfoundation.common.modifiers.state.box.{AssetBox, EncryBaseBox, TokenIssuingBox}
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.Height
import org.encryfoundation.common.utils.constants.TestNetConstants
import org.encryfoundation.prismlang.compiler.CompiledContract
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{AsyncFunSuite, Matchers}
import scorex.crypto.signatures.Curve25519
import scorex.utils.Random

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class ElectionContract extends AsyncFunSuite
  with Matchers
  with ScalaFutures
  with StrictLogging
  with NodesFromDocker {

  override protected def nodeConfigs: Seq[Config] = Seq(Configs.mining(true)
    .withFallback(Configs.offlineGeneration(true))
    .withFallback(Configs.nodeName("node1")))

  test("election contract") {

    val waitingTime: FiniteDuration = 1.minutes
    val mnemonicKey: String = "index another island accuse valid aerobic little absurd bunker keep insect scissors"
    val privateKey: PrivateKey25519 = createPrivKey(Some(mnemonicKey))

    val firstUser = "observe fee divorce bike strategy rubber later raven wrap effort lend slam"
    val secondUser = "crystal apple middle glow bind owner attend vast cliff naive first river"
    val firstPrivateKey: PrivateKey25519 = createPrivKey(Some(firstUser))
    val secondPrivateKey = createPrivKey(Some(secondUser))

    val heightForPreMining: Int = 3
    val heightForMiningAssetIssuingTransactions: Int = 6
    val heightForMiningTransferTransactions: Int = 9

    Await.result(dockerNodes().head.waitForHeadersHeight(heightForPreMining), waitingTime)

    val preMinedBoxes: Seq[EncryBaseBox] = Await.result(dockerNodes().head.outputs, waitingTime)
    val boxForAssetIssuingTransactions: AssetBox = preMinedBoxes.collect { case ab: AssetBox => ab }.head
    val transaction: Transaction = CreateTransaction.assetIssuingTransactionScratch(
      privateKey,
      10,
      System.currentTimeMillis(),
      Seq(boxForAssetIssuingTransactions -> None),
      PubKeyLockedContract(privateKey.publicImage.pubKeyBytes).contract,
      100
    )

    val ts = transaction

    Await.result(dockerNodes().head.sendTransaction(transaction), waitingTime)
    Await.result(dockerNodes().head.waitForHeadersHeight(heightForMiningAssetIssuingTransactions), waitingTime)

    val headersAtHeight: List[String] = (heightForPreMining to heightForMiningAssetIssuingTransactions)
      .foldLeft(List[String]()) { case (list, blockHeight) =>
        val headers: Future[List[String]] = dockerNodes().head.getHeadersIdAtHeight(blockHeight)
        val result: List[String] = Await.result(headers, waitingTime)
        list ::: result
      }

    val blocks: List[Block] =
      Await.result(Future.sequence(headersAtHeight.map { h => dockerNodes().head.getBlock(h) }), 1.minutes)

    val tokenId: TokenId = blocks.collect {
      case block: Block if block.payload.txs.size > 1 =>
        block.payload.txs.collect {
          case transaction: Transaction if transaction.fee > 0 =>
            transaction.newBoxes.toList.collect { case box: TokenIssuingBox => box.tokenId }
        }.flatten
    }.flatten.head

    val checkBalance: Boolean = Await.result(dockerNodes().head.balances, waitingTime)
      .find(_._1 == Algos.encode(tokenId))
      .map(_._2 == 1000)
      .get

    checkBalance shouldBe true

    val newBoxes: Seq[EncryBaseBox] = Await.result(dockerNodes().head.outputs, waitingTime)
    val tokenBox: TokenIssuingBox = newBoxes.collect { case tb: TokenIssuingBox => tb }.head

    val boxForFirstTransferAssetTx: AssetBox = preMinedBoxes.collect { case ab: AssetBox => ab }.toList(2)
    val boxForSecondTransferAssetTx: AssetBox = preMinedBoxes.collect { case ab: AssetBox => ab }.toList(3)
    val txForFirstUser: Transaction = CreateTransaction.defaultPaymentTransaction(
      privateKey,
      10,
      System.currentTimeMillis(),
      IndexedSeq(boxForFirstTransferAssetTx, tokenBox).map(_ -> None),
      firstPrivateKey.publicImage.address.address,
      50,
      Map(tokenId -> 50)
    )
    val txForSecondUser: Transaction = CreateTransaction.defaultPaymentTransaction(
      privateKey,
      10,
      System.currentTimeMillis(),
      IndexedSeq(boxForSecondTransferAssetTx, tokenBox).map(_ -> None),
      secondPrivateKey.publicImage.address.address,
      50,
      Map(tokenId -> 50)
    )

    Await.result(dockerNodes().head.sendTransaction(txForFirstUser), waitingTime)
    Await.result(dockerNodes().head.sendTransaction(txForSecondUser), waitingTime)
    Await.result(dockerNodes().head.waitForHeadersHeight(heightForMiningTransferTransactions), waitingTime)

    val headersAtHeightAfterAssetTxSending: List[String] =
      (heightForMiningAssetIssuingTransactions to heightForMiningTransferTransactions)
        .foldLeft(List[String]()) { case (list, blockHeight) =>
          val headers: Future[List[String]] = dockerNodes().head.getHeadersIdAtHeight(blockHeight)
          val result: List[String] = Await.result(headers, waitingTime)
          list ::: result
        }

    val blocksAfterAssetTxSending: List[Block] = Await.result(
      Future.sequence(headersAtHeightAfterAssetTxSending.map { h => dockerNodes().head.getBlock(h) }),
      1.minutes
    )



    Future(1 shouldEqual 1)
  }

  test("") {

    val firstHeightToWait: Int = 2
    val secondHeightToWait: Int = 6
    val thirdHeightToWait: Int = 10
    val mnemonicKey: String = "index another island accuse valid aerobic little absurd bunker keep insect scissors"
    val privKey: PrivateKey25519 = createPrivKey(Some(mnemonicKey))
    val waitTime: FiniteDuration = 30.minutes
    val amount: Int = 2000
    val fee: Int = 1
    val createdTokensAmount: Int = 100
    val tokenAmount: Int = 1
    val recipientAddress: Address = PublicKey25519(Curve25519.createKeyPair(Random.randomBytes())._2).address.address

    Await.result(dockerNodes().head.waitForHeadersHeight(firstHeightToWait), waitTime)

    val initBoxes: Seq[EncryBaseBox] = Await.result(dockerNodes().head.outputs, waitTime)
    val oneBox: AssetBox = initBoxes.collect { case ab: AssetBox => ab }.head
    val transaction: Transaction = CreateTransaction.assetIssuingTransactionScratch(
      privKey,
      fee,
      timestamp = System.currentTimeMillis(),
      useOutputs = IndexedSeq(oneBox).map(_ -> None),
      contract = PubKeyLockedContract(privKey.publicImage.pubKeyBytes).contract,
      createdTokensAmount
    )

    Await.result(dockerNodes().head.sendTransaction(transaction), waitTime)
    Await.result(dockerNodes().head.waitForHeadersHeight(secondHeightToWait), waitTime)

    val headersAtHeight: List[String] = (firstHeightToWait + 1 to secondHeightToWait).foldLeft(List[String]()) {
      case (list, blockHeight) =>
        val headers: Future[List[String]] = dockerNodes().head.getHeadersIdAtHeight(blockHeight)
        val result: List[String] = Await.result(headers, waitTime)
        list ::: result
    }

    Future.sequence(headersAtHeight.map { h => dockerNodes().head.getBlock(h) }).flatMap { blockByHeaders =>

      val tokenId: TokenId = blockByHeaders.collect {
        case block: Block if block.payload.txs.size > 1 =>
          block.payload.txs.collect {
            case transaction: Transaction if transaction.fee > 0 =>
              transaction.newBoxes.toList.collect { case box: TokenIssuingBox => box.tokenId }
          }.flatten
      }.flatten.head

      val checkBalance: Boolean = Await.result(dockerNodes().head.balances, waitTime)
        .find(_._1 == Algos.encode(tokenId))
        .map(_._2 == createdTokensAmount)
        .get

      val txsNum: Int = blockByHeaders.map(_.payload.txs.size).sum
      val transactionFromChain: Transaction = blockByHeaders.flatMap(_.payload.txs.init).head

      transactionFromChain.id shouldEqual transaction.id
      checkBalance shouldEqual true
      txsNum shouldEqual (secondHeightToWait - firstHeightToWait + 1)

      val getBoxesAgain: Seq[EncryBaseBox] = Await.result(dockerNodes().head.outputs, waitTime)

      val assetBoxForFee: AssetBox = getBoxesAgain.collect {
        case ab: AssetBox if ab.amount >= amount + fee => ab
      }.head
      val tokenIssuingBox: TokenIssuingBox = getBoxesAgain.collect {
        case tb: TokenIssuingBox if tb.amount >= tokenAmount => tb
      }.head

      val transactionWithAssetToken: Transaction = CreateTransaction.defaultPaymentTransaction(
        privKey,
        fee,
        System.currentTimeMillis(),
        IndexedSeq(assetBoxForFee, tokenIssuingBox).map(_ -> None),
        recipientAddress,
        amount,
        Map(tokenId -> tokenAmount)
      )

      Await.result(dockerNodes().head.sendTransaction(transactionWithAssetToken), waitTime)
      Await.result(dockerNodes().head.waitForHeadersHeight(thirdHeightToWait), waitTime)

      val headersAtHeightNew: List[String] = (secondHeightToWait + 1 to thirdHeightToWait)
        .foldLeft(List[String]()) { case (list, blockHeight) =>
          val headers: Future[List[String]] = dockerNodes().head.getHeadersIdAtHeight(blockHeight)
          val result: List[String] = Await.result(headers, waitTime)
          list ::: result
        }

      Future.sequence(headersAtHeightNew.map { h => dockerNodes().head.getBlock(h) }).map { blockByHeadersNew =>

        val checkTokenBalance: Boolean = Await.result(dockerNodes().head.balances, waitTime)
          .find(_._1 == Algos.encode(tokenId))
          .map(_._2 == createdTokensAmount - tokenAmount)
          .get

        val supplyAtHeight: Long = (0 to thirdHeightToWait).foldLeft(0: Long) {
          case (supply, i) => supply + EncrySupplyController.supplyAt(Height @@ i)
        }

        val ckeckEncryBalanceNew: Boolean = Await.result(dockerNodes().head.balances, waitTime)
          .find(_._1 == Algos.encode(TestNetConstants.IntrinsicTokenId))
          .map(_._2 == supplyAtHeight - amount)
          .get

        val txsNumNew: Int = blockByHeadersNew.map(_.payload.txs.size).sum

        docker.close()
        val transactionFromChainNew: Transaction = blockByHeadersNew.flatMap(_.payload.txs.init).head

        transactionFromChainNew.id shouldEqual transactionWithAssetToken.id
        ckeckEncryBalanceNew shouldEqual true
        checkTokenBalance shouldEqual true
        txsNumNew shouldEqual (thirdHeightToWait - secondHeightToWait + 1)
      }
    }
  }
}
