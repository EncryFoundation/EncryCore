package encry.it.transactions

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
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.modifiers.state.box.{AssetBox, EncryBaseBox}
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.Height
import encry.settings.EncryAppSettings.read.constants
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{AsyncFunSuite, Matchers}
import scorex.crypto.signatures.Curve25519
import scorex.utils.Random

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class ProcessingTransferTransactionWithEncryCoinsTest extends AsyncFunSuite
  with Matchers
  with ScalaFutures
  with StrictLogging
  with NodesFromDocker {

  override protected def nodeConfigs: Seq[Config] = Seq(Configs.mining(true)
    .withFallback(Configs.offlineGeneration(true))
    .withFallback(Configs.nodeName("node1")))

  test("Create and send monetary transaction. Check balance.") {

    val amount: Int = scala.util.Random.nextInt(2000)
    val fee: Long = scala.util.Random.nextInt(500)
    val firstHeightToWait: Int = 5
    val secondHeightToWait: Int = 8
    val mnemonicKey: String = "index another island accuse valid aerobic little absurd bunker keep insect scissors"
    val privKey: PrivateKey25519 = createPrivKey(Some(mnemonicKey))
    val recipientAddress: Address = PublicKey25519(Curve25519.createKeyPair(Random.randomBytes())._2).address.address
    val waitTime: FiniteDuration = 30.minutes

    val supplyAtHeight: Long = (0 to secondHeightToWait).foldLeft(0: Long) {
      case (supply, i) => supply + EncrySupplyController.supplyAt(Height @@ i)
    }

    Await.result(dockerNodes().head.waitForHeadersHeight(firstHeightToWait), waitTime)

    val boxes: Seq[EncryBaseBox] = Await.result(dockerNodes().head.outputs, waitTime)
    val oneBox: AssetBox = boxes.collect { case ab: AssetBox => ab }.head
    val transaction: Transaction = CreateTransaction.defaultPaymentTransaction(
      privKey,
      fee,
      System.currentTimeMillis(),
      IndexedSeq(oneBox).map(_ -> None),
      recipientAddress,
      amount
    )

    Await.result(dockerNodes().head.sendTransaction(transaction), waitTime)
    Await.result(dockerNodes().head.waitForHeadersHeight(secondHeightToWait), waitTime)

    val checkBalance: Boolean = Await.result(dockerNodes().head.balances, waitTime)
      .find(_._1 == Algos.encode(constants.IntrinsicTokenId))
      .map(_._2 == supplyAtHeight - amount)
      .get

    val headersAtHeight: List[String] = (firstHeightToWait + 1 to secondHeightToWait)
      .foldLeft(List[String]()) { case (list, blockHeight) =>
        val headers: Future[List[String]] = dockerNodes().head.getHeadersIdAtHeight(blockHeight)
        val result: List[String] = Await.result(headers, waitTime)
        list ::: result
      }

    Await.result(dockerNodes().head.getBlock(headersAtHeight.head), waitTime)

    val lastBlocks: Future[Seq[Block]] = Future.sequence(headersAtHeight.map { h => dockerNodes().head.getBlock(h) })

    lastBlocks.map { blocks =>
      val txsNum: Int = blocks.map(_.payload.txs.size).sum
      docker.close()
      val transactionFromChain: Transaction = blocks.flatMap(_.payload.txs.init).head
      transactionFromChain.id shouldEqual transaction.id
      true shouldEqual (txsNum > secondHeightToWait - firstHeightToWait)
      txsNum shouldEqual (secondHeightToWait - firstHeightToWait + 1)
      checkBalance shouldBe true
    }
  }
}