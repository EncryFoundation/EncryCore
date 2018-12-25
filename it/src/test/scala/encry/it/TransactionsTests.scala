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
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import org.encryfoundation.common.Algos

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.Future

class TransactionsTests extends AsyncFunSuite with Matchers with ScalaFutures with StrictLogging {

  test("Get box, form and send transaction. Check block for availability of this transaction. Check miner balance.") {

    val heightToCheck: Int = 5
    val heightToCheckNew: Int = 8
    val mnemonicKey: String = "index another island accuse valid aerobic little absurd bunker keep insect scissors"
    val privKey: PrivateKey25519 = createPrivKey(Some(mnemonicKey))
    logger.info(Algos.encode(privKey.privKeyBytes) + " dksfmksdfmkfksdkfkdskkd")
    val recipientAddress: Address = PublicKey25519(Curve25519.createKeyPair(Random.randomBytes())._2).address.address

    val docker: Docker = Docker()
    val config: Config = Configs.mining(true)
      .withFallback(Configs.offlineGeneration(true))
      .withFallback(Configs.nodeName("node1"))

    val nodes: List[Node] = docker.startNodes(Seq(config))
    val height: Future[Int] = nodes.head.waitForHeadersHeight(heightToCheck)

    val res1: Int = Await.result(height, 2.minutes)

    val boxes: Future[Seq[EncryBaseBox]] = nodes.head.outputs

    val res2: Seq[EncryBaseBox] = Await.result(boxes, 2.minutes)

    val box: AssetBox = res2.collect {
      case ab: AssetBox => ab
    }.head

    logger.info(box.toString)

    val transaction = CreateTransaction.defaultPaymentTransaction(
      privKey,
      101,
      System.currentTimeMillis(),
      IndexedSeq(box).map(_ -> None),
      recipientAddress,
      102
    )

    logger.info(transaction.asJson.toString())

    val sendTransaction: Future[Unit] = nodes.head.sendTransaction(transaction)

    logger.info(123.toString)

    val resTmp: Unit = Await.result(sendTransaction, 2.minutes)

    logger.info(456.toString)

    val heightNew: Future[Int] = nodes.head.waitForHeadersHeight(heightToCheckNew)

    val res3: Int = Await.result(heightNew, 2.minutes)

    logger.info(res3.toString)

    val lastHeaders: List[String] =
      (heightToCheck + 1 to heightToCheckNew).foldLeft(List[String]()) { case (a, b) =>
        val headers: Future[List[String]] = nodes.head.getHeadersIdAtHeight(b)
        val result: List[String] = Await.result(headers, 1.minutes)
        a ::: result
      }

    val test = nodes.head.getBlock(lastHeaders.head)

    val qwer = Await.result(test, 1.minutes)

    logger.info(qwer.toString)

    val lastBlocks: Future[Seq[Block]] = Future.sequence(lastHeaders.map { h => nodes.head.getBlock(h) })

    lastBlocks.map { blocks =>
      val txsNum: Int = blocks.map(_.payload.transactions.size).sum
      docker.close()
      true shouldEqual (txsNum > 3)
      txsNum shouldEqual 4
    }
  }
}