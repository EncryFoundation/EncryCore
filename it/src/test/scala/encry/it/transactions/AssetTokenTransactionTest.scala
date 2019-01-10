package encry.it.transactions

import TransactionGenerator.CreateTransaction
import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import encry.it.configs.Configs
import encry.it.docker.{Docker, Node}
import encry.it.util.KeyHelper._
import encry.modifiers.history.Block
import encry.modifiers.mempool.Transaction
import encry.modifiers.state.box.TokenIssuingBox.TokenId
import encry.modifiers.state.box._
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

class AssetTokenTransactionTest extends AsyncFunSuite with Matchers with ScalaFutures with StrictLogging {

  test("Create and send asset token transaction. Check new token's balance." +
    " Send tokens. Check new balance.") {

    val heightToCheckFirst: Int   = 5
    val heightToCheckSecond: Int  = 8
    val heightToCheckThird: Int   = 11
    val mnemonicKey: String       = "index another island accuse valid aerobic little absurd bunker keep insect scissors"
    val privKey: PrivateKey25519  = createPrivKey(Some(mnemonicKey))
    val waitTime: FiniteDuration  = 1.minutes
    val amount: Int               = 1001
    val fee: Int                  = 101
    val tokenAmount: Int          = 101
    val recipientAddress: Address = PublicKey25519(Curve25519.createKeyPair(Random.randomBytes())._2).address.address

    val docker: Docker = Docker()
    val config: Config = Configs.mining(true)
      .withFallback(Configs.offlineGeneration(true))
      .withFallback(Configs.nodeName("node2"))
    val nodes: List[Node] = docker.startNodes(Seq(config))

    Await.result(nodes.head.waitForHeadersHeight(heightToCheckFirst), waitTime)

    val getBoxes: Seq[EncryBaseBox] = Await.result(nodes.head.outputs, waitTime)
    val oneBox: AssetBox            = getBoxes.collect { case ab: AssetBox => ab }.head
    val transaction: Transaction    = CreateTransaction.assetIssuingTransactionScratch(
      privKey,
      fee,
      timestamp = System.currentTimeMillis(),
      useOutputs = IndexedSeq(oneBox).map(_ -> None),
      contract = PubKeyLockedContract(privKey.publicImage.pubKeyBytes).contract,
      amount,
    )

    Await.result(nodes.head.sendTransaction(transaction), waitTime)
    Await.result(nodes.head.waitForHeadersHeight(heightToCheckSecond), waitTime)

    val headersAtHeight: List[String] = (heightToCheckFirst + 1 to heightToCheckSecond).foldLeft(List[String]()) {
      case (list, blockHeight) =>
        val headers: Future[List[String]] = nodes.head.getHeadersIdAtHeight(blockHeight)
        val result: List[String]          = Await.result(headers, waitTime)
        list ::: result
      }

    Future.sequence(headersAtHeight.map { h => nodes.head.getBlock(h) }).flatMap { blockByHeaders =>

      val tokenId: TokenId = blockByHeaders.collect {
        case block: Block if block.transactions.size > 1 =>
          block.transactions.collect {
            case transaction: Transaction if transaction.fee > 0 =>
              transaction.newBoxes.toList.collect { case box: TokenIssuingBox => box.tokenId }
          }.flatten
      }.flatten.head

      val balance: Boolean = Await.result(nodes.head.balances, waitTime)
        .find(_._1 == Algos.encode(tokenId))
        .map(_._2 == amount)
        .get

      val txsNum: Int = blockByHeaders.map(_.payload.transactions.size).sum

      balance shouldEqual true
      true shouldEqual (txsNum > heightToCheckSecond - heightToCheckFirst)
      txsNum shouldEqual (heightToCheckSecond - heightToCheckFirst + 1)

      val getBoxesAgain: Seq[EncryBaseBox] = Await.result(nodes.head.outputs, waitTime)

      val assetBoxForFee: AssetBox         = getBoxesAgain.collect { case ab: AssetBox if ab.amount > amount => ab }.head
      val tokenIssuingBox: TokenIssuingBox = getBoxesAgain.collect { case tb: TokenIssuingBox if tb.amount > fee => tb }.head

      val transactionWithAssetToken: Transaction = CreateTransaction.defaultPaymentTransaction(
        privKey,
        fee,
        System.currentTimeMillis(),
        IndexedSeq(assetBoxForFee, tokenIssuingBox).map(_ -> None),
        recipientAddress,
        amount,
        Map(tokenId -> tokenAmount)
      )

      Await.result(nodes.head.sendTransaction(transactionWithAssetToken), waitTime)
      Await.result(nodes.head.waitForHeadersHeight(heightToCheckThird), waitTime)

      val headersAtHeightNew: List[String] = (heightToCheckSecond + 1 to heightToCheckThird)
        .foldLeft(List[String]()) { case (list, blockHeight) =>
          val headers: Future[List[String]] = nodes.head.getHeadersIdAtHeight(blockHeight)
          val result: List[String] = Await.result(headers, waitTime)
          list ::: result
        }

      Future.sequence(headersAtHeightNew.map { h => nodes.head.getBlock(h) }).map { blockByHeadersNew =>

        val balanceNew: Boolean = Await.result(nodes.head.balances, waitTime)
          .find(_._1 == Algos.encode(tokenId))
          .map(_._2 == amount - tokenAmount)
          .get

        val txsNumNew: Int = blockByHeadersNew.map(_.payload.transactions.size).sum

        docker.close()

        balanceNew shouldEqual true
        true shouldEqual (txsNumNew > heightToCheckThird - heightToCheckSecond)
        txsNumNew shouldEqual (heightToCheckThird - heightToCheckSecond + 1)
      }
    }
  }
}