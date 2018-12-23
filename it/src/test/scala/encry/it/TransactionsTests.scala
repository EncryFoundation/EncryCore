package encry.it

import com.typesafe.config.Config
import encry.consensus.EncrySupplyController
import encry.it.configs.Configs
import encry.it.docker.{Docker, Node}
import encry.modifiers.mempool.{Transaction, TransactionFactory}
import encry.modifiers.state.box.{AssetBox, EncryBaseBox}
import encry.settings.Constants._
import encry.view.history.History.Height
import org.encryfoundation.common.Algos
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{AsyncFunSuite, Matchers}
import encry.it.util.KeyHelper._
import encry.modifiers.history.{Block, Header}
import org.encryfoundation.common.crypto.{PrivateKey25519, PublicKey25519}
import org.encryfoundation.common.transaction.EncryAddress.Address
import scorex.crypto.encode.Base16
import scorex.crypto.signatures.{Curve25519, PrivateKey, PublicKey}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.Future

class TransactionsTests extends AsyncFunSuite with Matchers with ScalaFutures {

  test("Get box, form and send transaction. Check block for availability of this transaction. Check miner balance.") {

    val heightToCheck: Int        = 5
    val heightToCheckNew: Int     = 2
    val mnemonicKey: String       = "index another island accuse valid aerobic little absurd bunker keep insect scissors"
    val privKey: PrivateKey25519  = createPrivKey(Some(mnemonicKey))
    val recipientAddress: Address = PublicKey25519(Curve25519.createKeyPair._2).address.address

    val docker: Docker = Docker()
    val config: Config = Configs.mining(true)
      .withFallback(Configs.offlineGeneration(true))
      .withFallback(Configs.mnemonicKey(mnemonicKey))

    val nodes: List[Node]   = docker.startNodes(Seq(config))
    val height: Future[Int] = nodes.head.waitForHeadersHeight(heightToCheck)

    Await.result(height, 4.minutes)

    val boxes: Future[Seq[EncryBaseBox]] = nodes.head.outputs

    Await.result(boxes, 4.minutes)

    val box: AssetBox = boxes.map(_.collect { case ab: AssetBox => ab }.head).futureValue

    val transaction: Transaction = TransactionFactory.defaultPaymentTransactionScratch(
      privKey,
      1,
      System.currentTimeMillis(),
      IndexedSeq(box),
      recipientAddress,
      1000
    )

    val sendTransaction: Future[Unit] = nodes.head.sendTransaction(transaction)

    Await.result(sendTransaction, 4.minutes)

    val heightNew: Future[Int] = nodes.head.waitForHeadersHeight(heightToCheckNew)

    Await.result(heightNew, 4.minutes)

    val lastHeaders: Future[Seq[Header]] = nodes.head.lastHeaders(heightToCheckNew)

    Await.result(lastHeaders, 4.minutes)

    val lastBlocks: Future[Seq[Block]] = Future.sequence(lastHeaders.futureValue.map { h =>
      nodes.head.getBlock(Base16.encode(h.id))
    })

    Await.result(lastBlocks, 4.minutes)

    Future {
      val numTxs: Int = lastBlocks.futureValue.map{ b =>
        b.payload.txs.size
      }.sum
      true shouldEqual (numTxs > 2)
      numTxs shouldEqual 3
    }
  }
}
