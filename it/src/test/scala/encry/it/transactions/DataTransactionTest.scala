package encry.it.transactions

import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import encry.it.configs.Configs
import encry.it.docker.NodesFromDocker
import encry.modifiers.mempool.TransactionFactory
import encry.utils.Keys
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.modifiers.mempool.transaction.{PubKeyLockedContract, Transaction}
import org.encryfoundation.common.modifiers.state.box.{AssetBox, EncryBaseBox}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{AsyncFunSuite, Matchers}
import scorex.utils.Random

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class DataTransactionTest extends AsyncFunSuite
  with Matchers
  with ScalaFutures
  with Keys
  with StrictLogging
  with NodesFromDocker {

  override protected def nodeConfigs: Seq[Config] = Seq(Configs.mining(true)
    .withFallback(Configs.offlineGeneration(true))
    .withFallback(Configs.nodeName("node1")))

  test("Create and send data transaction. Check chain for it.") {

    val firstHeightToWait: Int = 5
    val secondHeightToWait: Int = 8
    val waitTime: FiniteDuration = 30.minutes
    val fee: Long = scala.util.Random.nextInt(500)

    Await.result(dockerNodes().head.waitForHeadersHeight(firstHeightToWait), waitTime)

    val boxes: Seq[EncryBaseBox] = Await.result(dockerNodes().head.outputs, waitTime)
    val oneBox: AssetBox = boxes.collect { case ab: AssetBox => ab }.head
    val transaction: Transaction = TransactionFactory.dataTransactionScratch(
      privKey,
      fee,
      System.currentTimeMillis(),
      IndexedSeq(oneBox),
      0,
      Random.randomBytes(32)
    )

    Await.result(dockerNodes().head.sendTransaction(transaction), waitTime)
    Await.result(dockerNodes().head.waitForHeadersHeight(secondHeightToWait), waitTime)

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
    }
  }
}