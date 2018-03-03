package encry.local

import akka.actor.{Actor, ActorRef, ActorRefFactory, Cancellable, Props}
import encry.local.TransactionGenerator.{GeneratePaymentTransactions, StartGeneration, StopGeneration}
import encry.modifiers.mempool.{EncryBaseTransaction, EncryTransaction, TransactionFactory}
import encry.modifiers.state.box.AssetBox
import encry.settings.TestingSettings
import encry.view.history.EncryHistory
import encry.view.mempool.EncryMempool
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import scorex.core.LocalInterface.LocallyGeneratedTransaction
import scorex.core.NodeViewHolder.GetDataFromCurrentView
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.utils.{NetworkTimeProvider, ScorexLogging}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Random

class TransactionGenerator(viewHolder: ActorRef, settings: TestingSettings, timeProvider: NetworkTimeProvider)
  extends Actor with ScorexLogging {

  var txGenerator: Cancellable = _

  var isStarted = false

  private lazy val factory = TestHelper

  override def receive: Receive = {
    case StartGeneration =>
      if (!isStarted) {
        log.info("Starting transaction generation.")
        context.system.scheduler.scheduleOnce(1500.millis)(self ! GeneratePaymentTransactions)
      }

    case GeneratePaymentTransactions =>
      viewHolder ! GetDataFromCurrentView[EncryHistory, UtxoState, EncryWallet, EncryMempool,
        Seq[EncryTransaction]] { v =>
        if (v.pool.size < settings.keepPoolSize) {
          (0 until settings.keepPoolSize - v.pool.size).map { _ =>
            val secret = v.vault.keyManager.keys.head
            val pubKey = secret.publicImage
            val recipient = pubKey.address
            val fee = 15L
            val amount: Long = Random.nextInt(30) + 9
            val timestamp = timeProvider.time()
            if (v.vault.balance > 1000) {
              // Generate valid txs if vault's balance is enough.
              val boxes = v.vault.walletStorage.allBoxes.filter(_.isInstanceOf[AssetBox])
                .map(_.asInstanceOf[AssetBox]).foldLeft(Seq[AssetBox]()) {
                case (seq, box) => if (seq.map(_.amount).sum < (amount + fee)) seq :+ box else seq
              }

              TransactionFactory.defaultPaymentTransaction(secret, fee, timestamp, boxes, recipient, amount)
            } else {
              // Generate semantically valid but stateful-invalid txs otherwise.
              val boxes = IndexedSeq(factory.genAssetBox(pubKey.address))

              TransactionFactory.defaultPaymentTransaction(secret, fee, timestamp, boxes, recipient, amount)
            }
          }
        } else {
          Seq.empty
        }
      }

    case txs: Seq[EncryBaseTransaction]@unchecked =>
      txs.foreach { tx =>
        viewHolder ! LocallyGeneratedTransaction[Proposition, EncryBaseTransaction](tx)
      }

    case StopGeneration =>
      txGenerator.cancel()
  }
}

object TransactionGenerator {

  def props(viewHolder: ActorRef, settings: TestingSettings, timeProvider: NetworkTimeProvider): Props =
    Props(new TransactionGenerator(viewHolder, settings, timeProvider))

  def apply(viewHolder: ActorRef, settings: TestingSettings, timeProvider: NetworkTimeProvider)
           (implicit context: ActorRefFactory): ActorRef =
    context.actorOf(props(viewHolder, settings, timeProvider))

  def apply(viewHolder: ActorRef, settings: TestingSettings, timeProvider: NetworkTimeProvider, name: String)
           (implicit context: ActorRefFactory): ActorRef =
    context.actorOf(props(viewHolder, settings, timeProvider), name)

  case object StartGeneration

  case object GeneratePaymentTransactions

  case object StopGeneration
}
