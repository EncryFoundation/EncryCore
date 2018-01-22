package encry.local

import akka.actor.{Actor, ActorRef, Cancellable}
import encry.account.Address
import encry.local.TransactionGenerator.{FetchBoxes, StartGeneration, StopGeneration}
import encry.modifiers.mempool.{EncryBaseTransaction, PaymentTransaction}
import encry.settings.TestingSettings
import encry.view.history.EncryHistory
import encry.view.mempool.EncryMempool
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import scorex.core.LocalInterface.LocallyGeneratedTransaction
import scorex.core.NodeViewHolder.GetDataFromCurrentView
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.core.utils.{NetworkTime, NetworkTimeProvider, ScorexLogging}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Random

class TransactionGenerator(viewHolder: ActorRef, settings: TestingSettings, timeProvider: NetworkTimeProvider)
  extends Actor with ScorexLogging {

  var txGenerator: Cancellable = _

  var isStarted = false

  private lazy val factory = TestHelper
  private lazy val keys = factory.getOrGenerateKeys(factory.Props.keysFilePath)

  private var currentSlice = (0, 7)
  private var txsGenerated = 0

  override def receive: Receive = {
    case StartGeneration =>
      if (!isStarted) {
        log.info("Starting transaction generation.")
        context.system.scheduler.scheduleOnce(1500.millis)(self ! FetchBoxes)
      }

    case FetchBoxes =>
      viewHolder ! GetDataFromCurrentView[EncryHistory, UtxoState, EncryWallet, EncryMempool,
        Seq[EncryBaseTransaction]] { v =>
        if (v.pool.size < settings.keepPoolSize) {
          var keysSlice = Seq[PrivateKey25519]()
          if (currentSlice._2 <= TestHelper.Props.keysQty) {
            keysSlice = keys.slice(currentSlice._1, currentSlice._2)
            txsGenerated += currentSlice._2 - currentSlice._1
          } else {
            keysSlice = keys.slice(currentSlice._1, TestHelper.Props.keysQty)
            txsGenerated += TestHelper.Props.keysQty - currentSlice._1
          }
          val randShift = Random.nextInt(10) + 2
          currentSlice = (currentSlice._2, currentSlice._2 + randShift)
          keysSlice.map { key =>
            val proposition = key.publicImage
            val fee = factory.Props.txFee
            val timestamp = timeProvider.time()
            val useBoxes = IndexedSeq(factory.genAssetBox(Address @@ key.publicImage.address)).map(_.id)
            val outputs = IndexedSeq((Address @@ factory.Props.recipientAddr, factory.Props.boxValue))
            val sig = PrivateKey25519Companion.sign(
              key, PaymentTransaction.getMessageToSign(proposition, fee, timestamp, useBoxes, outputs))
            PaymentTransaction(proposition, fee, timestamp, sig, useBoxes, outputs)
          }
        } else {
          Seq()
        }
      }
      if (txsGenerated < TestHelper.Props.keysQty)
        log.info(s"$txsGenerated transactions generated, repeating in 5sec ...")
        context.system.scheduler.scheduleOnce(5.seconds)(self ! FetchBoxes)

    case txs: Seq[EncryBaseTransaction] =>
      txs.foreach { tx =>
        viewHolder ! LocallyGeneratedTransaction[Proposition, EncryBaseTransaction](tx)
      }

    case StopGeneration =>
      txGenerator.cancel()
  }
}

object TransactionGenerator {

  case object StartGeneration

  case object FetchBoxes

  case object StopGeneration
}
