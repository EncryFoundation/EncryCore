package encry.it.transactions

import TransactionGenerator.CreateTransaction
import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.EncrySupplyController
import encry.it.configs.Configs
import encry.it.docker.NodesFromDocker
import encry.it.util.KeyHelper._
import encry.modifiers.history.Block
import encry.modifiers.mempool.Transaction
import encry.modifiers.state.box.TokenIssuingBox.TokenId
import encry.modifiers.state.box._
import encry.settings.Constants.IntrinsicTokenId
import encry.view.history.History.Height
import org.encryfoundation.common.Algos
import org.encryfoundation.common.crypto.{PrivateKey25519, PublicKey25519}
import org.encryfoundation.common.transaction.EncryAddress.Address
import org.encryfoundation.common.transaction.PubKeyLockedContract
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{AsyncFunSuite, Matchers}
import scorex.crypto.signatures.Curve25519
import scorex.utils.Random
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class AssetTokenTransactionTest extends AsyncFunSuite
  with Matchers
  with ScalaFutures
  with StrictLogging
  with NodesFromDocker {

  override protected def nodeConfigs: Seq[Config] = Seq(Configs.mining(true)
    .withFallback(Configs.offlineGeneration(true))
    .withFallback(Configs.nodeName("node1")))

  test("Create and send asset token transaction. Check new token's balance." +
    " Send tokens. Check new balance") {

    val firstHeightToWait: Int = 5
    val secondHeightToWait: Int = 8
    val thirdHeightToWait: Int = 11
    val mnemonicKey: String = "index another island accuse valid aerobic little absurd bunker keep insect scissors"
    val privKey: PrivateKey25519 = createPrivKey(Some(mnemonicKey))
    val waitTime: FiniteDuration = 30.minutes
    val amount: Int = scala.util.Random.nextInt(2000)
    val fee: Int = scala.util.Random.nextInt(500)
    val createdTokensAmount: Int = scala.util.Random.nextInt(2000)
    val tokenAmount: Int = scala.util.Random.nextInt(500)
    val recipientAddress: Address = PublicKey25519(Curve25519.createKeyPair(Random.randomBytes())._2).address.address

    Await.result(dockerNodes().head.waitForHeadersHeight(firstHeightToWait), waitTime)

    val getBoxes: Seq[EncryBaseBox] = Await.result(dockerNodes().head.outputs, waitTime)
    val oneBox: AssetBox = getBoxes.collect { case ab: AssetBox => ab }.head
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
        case block: Block if block.transactions.size > 1 =>
          block.transactions.collect {
            case transaction: Transaction if transaction.fee > 0 =>
              transaction.newBoxes.toList.collect { case box: TokenIssuingBox => box.tokenId }
          }.flatten
      }.flatten.head

      val checkBalance: Boolean = Await.result(dockerNodes().head.balances, waitTime)
        .find(_._1 == Algos.encode(tokenId))
        .map(_._2 == createdTokensAmount)
        .get

      val txsNum: Int = blockByHeaders.map(_.payload.transactions.size).sum
      val transactionFromChain: Transaction = blockByHeaders.flatMap(_.payload.transactions.init).head

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
          .find(_._1 == Algos.encode(IntrinsicTokenId))
          .map(_._2 == supplyAtHeight - amount)
          .get

        val txsNumNew: Int = blockByHeadersNew.map(_.payload.transactions.size).sum

        docker.close()
        val transactionFromChainNew: Transaction = blockByHeadersNew.flatMap(_.payload.transactions.init).head

        transactionFromChainNew.id shouldEqual transactionWithAssetToken.id
        ckeckEncryBalanceNew shouldEqual true
        checkTokenBalance shouldEqual true
        txsNumNew shouldEqual (thirdHeightToWait - secondHeightToWait + 1)
      }
    }
  }
}