package encry.it.transactions

import TransactionGenerator.CreateTransaction
import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import encry.it.configs.Configs
import encry.it.docker.{Docker, Node}
import encry.it.util.KeyHelper._
import encry.modifiers.history.Block
import encry.modifiers.mempool.Transaction
import encry.modifiers.state.box.{AssetBox, EncryBaseBox}
import org.encryfoundation.common.crypto.PrivateKey25519
import org.encryfoundation.common.transaction.PubKeyLockedContract
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{AsyncFunSuite, Matchers}
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class AssetTockenTransactionTest extends AsyncFunSuite with Matchers with ScalaFutures with StrictLogging {

  test("Get box, form and send assetIssue transaction. Check block for availability of this transaction.") {

    println(Docker.configTemplate + " wkjefjnksnlfnlwekjnfl123k")

    val heightToCheckFirst: Int   = 5
    val heightToCheckSecond: Int  = 8
    val mnemonicKey: String       = "index another island accuse valid aerobic little absurd bunker keep insect scissors"
    val privKey: PrivateKey25519  = createPrivKey(Some(mnemonicKey))

    val docker: Docker = Docker()
    val config: Config = Configs.mining(true)
      .withFallback(Configs.offlineGeneration(true))
      .withFallback(Configs.nodeName("node2"))

    val nodes: List[Node]   = docker.startNodes(Seq(config))
    val height: Future[Int] = nodes.head.waitForHeadersHeight(heightToCheckFirst)

    Await.result(height, 2.minutes)

    val boxes: Future[Seq[EncryBaseBox]] = nodes.head.outputs
    val resultOne: Seq[EncryBaseBox]     = Await.result(boxes, 2.minutes)

    val oneBox: AssetBox         = resultOne.collect { case ab: AssetBox => ab }.head
    val transaction: Transaction = CreateTransaction.assetIssuingTransactionScratch(
      privKey,
      fee        = 101,
      timestamp  = System.currentTimeMillis(),
      useOutputs = IndexedSeq(oneBox).map(_ -> None),
      contract   = PubKeyLockedContract(privKey.publicImage.pubKeyBytes).contract,
      amount     = 1001,
    )

    val sendTransaction: Future[Unit] = nodes.head.sendTransaction(transaction)
    Await.result(sendTransaction, 2.minutes)

    val heightNew: Future[Int] = nodes.head.waitForHeadersHeight(heightToCheckSecond)
    Await.result(heightNew, 2.minutes)

    val lastHeaders: List[String] =
      (heightToCheckFirst + 1 to heightToCheckSecond).foldLeft(List[String]()) { case (list, blockHeight) =>
        val headers: Future[List[String]] = nodes.head.getHeadersIdAtHeight(blockHeight)
        val result: List[String]          = Await.result(headers, 1.minutes)
        list ::: result
      }

    val test = nodes.head.getBlock(lastHeaders.head)
    Await.result(test, 1.minutes)

    val lastBlocks: Future[Seq[Block]] = Future.sequence(lastHeaders.map { h => nodes.head.getBlock(h) })

    lastBlocks.map { blocks =>
      val txsNum: Int = blocks.map(_.payload.transactions.size).sum
      docker.close()
      true shouldEqual (txsNum > 3)
      txsNum shouldEqual 4
    }
  }
}