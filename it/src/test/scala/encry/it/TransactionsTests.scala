package encry.it

import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import encry.it.configs.Configs
import encry.it.docker.{Docker, Node}
import encry.modifiers.mempool.{Transaction, TransactionFactory}
import encry.modifiers.state.box.{AssetBox, EncryBaseBox, MonetaryBox}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{AsyncFunSuite, Matchers}
import encry.it.util.KeyHelper._
import encry.modifiers.history.{Block, Header}
import org.encryfoundation.common.crypto.{PrivateKey25519, PublicKey25519}
import org.encryfoundation.common.transaction.EncryAddress.Address
import scorex.crypto.encode.Base16
import scorex.crypto.signatures.Curve25519
import scorex.utils.Random

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.Future

class TransactionsTests extends AsyncFunSuite with Matchers with ScalaFutures with StrictLogging {

  test("Get box, form and send transaction. Check block for availability of this transaction. Check miner balance.") {

    val heightToCheck: Int = 5
    val heightToCheckNew: Int = 2
    val mnemonicKey: String = "index another island accuse valid aerobic little absurd bunker keep insect scissors"
    val privKey: PrivateKey25519 = createPrivKey(Some(mnemonicKey))
    val recipientAddress: Address = PublicKey25519(Curve25519.createKeyPair(Random.randomBytes())._2).address.address

    val docker: Docker = Docker()
    val config: Config = Configs.mining(true)
      .withFallback(Configs.offlineGeneration(true))
      .withFallback(Configs.mnemonicKey(mnemonicKey))
      .withFallback(Configs.nodeName("node1"))

    val nodes: List[Node] = docker.startNodes(Seq(config))
    val height: Future[Int] = nodes.head.waitForHeadersHeight(heightToCheck)

    val res1: Int = Await.result(height, 4.minutes)

    val boxes: Future[Seq[EncryBaseBox]] = nodes.head.outputs

    val res2: Seq[EncryBaseBox] = Await.result(boxes, 4.minutes)

    val box: MonetaryBox = res2.head.asInstanceOf[MonetaryBox]

    logger.info(box.toString)

    val transaction: Transaction = TransactionFactory.defaultPaymentTransactionScratch(
      privKey,
      1,
      System.currentTimeMillis(),
      IndexedSeq(box),
      recipientAddress,
      1000
    )

    logger.info(transaction.toString)

    val sendTransaction: Future[Unit] = nodes.head.sendTransaction(transaction)

    logger.info(123.toString)

    val resTmp: Unit = Await.result(sendTransaction, 4.minutes)

    logger.info(456.toString)

    val heightNew: Future[Int] = nodes.head.waitForHeadersHeight(heightToCheckNew)

    val res3: Int = Await.result(heightNew, 4.minutes)

    logger.info(res3.toString)

    val lastHeaders: Future[Seq[Header]] = nodes.head.lastHeaders(heightToCheckNew)

    val res4: Seq[Header] = Await.result(lastHeaders, 4.minutes)

    logger.info(res4.toString())

    val lastBlocks: Future[Seq[Block]] = Future.sequence(res4.map { h =>
      nodes.head.getBlock(Base16.encode(h.id))
    })

    val res5: Seq[Block] = Await.result(lastBlocks, 4.minutes)

    Future {
      val numTxs: Int = res5.map { b =>
        b.payload.txs.size
      }.sum
      docker.close()
      true shouldEqual (numTxs > 2)
      numTxs shouldEqual 3
    }
  }
}