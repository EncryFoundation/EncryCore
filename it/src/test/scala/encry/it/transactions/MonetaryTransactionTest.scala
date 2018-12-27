package encry.it.transactions

import TransactionGenerator.CreateTransaction
import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.EncrySupplyController
import encry.it.configs.Configs
import encry.it.docker.{Docker, Node}
import encry.it.util.KeyHelper._
import encry.modifiers.history.Block
import encry.modifiers.mempool.Transaction
import encry.modifiers.state.box.{AssetBox, EncryBaseBox}
import encry.settings.Constants.IntrinsicTokenId
import encry.view.history.History.Height
import org.encryfoundation.common.Algos
import org.encryfoundation.common.crypto.{PrivateKey25519, PublicKey25519}
import org.encryfoundation.common.transaction.EncryAddress.Address
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{AsyncFunSuite, Matchers}
import scorex.crypto.signatures.Curve25519
import scorex.utils.Random
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class MonetaryTransactionTest extends AsyncFunSuite with Matchers with ScalaFutures with StrictLogging {

  test("Get box, form and send monetary transaction. Check block for availability of this transaction. " +
    "Check balance after sending transaction") {

    println(Docker.configTemplate + " wkjefjnksnlfnlwekjnfl123k")

    val amount: Int               = 1001
    val heightToCheckFirst: Int   = 5
    val heightToCheckSecond: Int  = 8
    val mnemonicKey: String       = "index another island accuse valid aerobic little absurd bunker keep insect scissors"
    val privKey: PrivateKey25519  = createPrivKey(Some(mnemonicKey))
    val recipientAddress: Address = PublicKey25519(Curve25519.createKeyPair(Random.randomBytes())._2).address.address

    val supplyAtHeight: Long = (0 to heightToCheckSecond).foldLeft(0: Long) {
      case (supply, i) => supply + EncrySupplyController.supplyAt(Height @@ i)
    }

    val docker: Docker = Docker()
    val config: Config = Configs.mining(true)
      .withFallback(Configs.offlineGeneration(true))
      .withFallback(Configs.nodeName("node1"))

    val nodes: List[Node]   = docker.startNodes(Seq(config))
    val height: Future[Int] = nodes.head.waitForHeadersHeight(heightToCheckFirst)

    Await.result(height, 2.minutes)

    val boxes: Future[Seq[EncryBaseBox]] = nodes.head.outputs
    val resultOne: Seq[EncryBaseBox]     = Await.result(boxes, 2.minutes)

    val oneBox: AssetBox         = resultOne.collect { case ab: AssetBox => ab }.head
    val transaction: Transaction = CreateTransaction.defaultPaymentTransaction(
      privKey,
      fee        = 101,
      timestamp  = System.currentTimeMillis(),
      useOutputs = IndexedSeq(oneBox).map(_ -> None),
      recipient  = recipientAddress,
      amount
    )

    val sendTransaction: Future[Unit] = nodes.head.sendTransaction(transaction)
    Await.result(sendTransaction, 2.minutes)

    val heightNew: Future[Int] = nodes.head.waitForHeadersHeight(heightToCheckSecond)
    Await.result(heightNew, 2.minutes)

    val checkBalance: Boolean = Await.result(nodes.head.balances, 4.minutes)
      .find(_._1 == Algos.encode(IntrinsicTokenId))
      .map(_._2 == supplyAtHeight - amount)
      .get

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
      true shouldEqual (txsNum > heightToCheckSecond - heightToCheckFirst)
      txsNum shouldEqual (heightToCheckSecond - heightToCheckFirst + 1)
      checkBalance shouldBe true
    }
  }
}