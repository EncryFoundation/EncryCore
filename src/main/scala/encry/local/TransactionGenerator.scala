package encry.local

import akka.actor.{Actor, ActorRef, Cancellable}
import encry.crypto.Address
import encry.local.TransactionGenerator.{FetchBoxes, StartGeneration, StopGeneration}
import encry.modifiers.mempool.{EncryBaseTransaction, PaymentTransaction}
import encry.settings.TestingSettings
import encry.view.history.EncryHistory
import encry.view.mempool.EncryMempool
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import scorex.core.LocalInterface.LocallyGeneratedTransaction
import scorex.core.NodeViewHolder.GetDataFromCurrentView
import scorex.core.transaction.box.Box.Amount
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.PrivateKey25519Companion
import scorex.core.utils.{NetworkTime, ScorexLogging}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

// TODO: Undone
class TransactionGenerator(viewHolder: ActorRef, settings: TestingSettings) extends Actor with ScorexLogging {
  var txGenerator: Cancellable = _

  var isStarted = false

  private lazy val factory = TransactionFactory
  private lazy val keys = factory.getOrGenerateKeys(factory.TestProps.keysFilePath)

  override def receive: Receive = {
    case StartGeneration =>
      if (!isStarted) {
        log.info("Starting transaction generation.")
        context.system.scheduler.schedule(1500.millis, 1500.millis)(self ! FetchBoxes)
      }

    case FetchBoxes =>
      viewHolder ! GetDataFromCurrentView[EncryHistory, UtxoState, EncryWallet, EncryMempool,
        Seq[EncryBaseTransaction]] { v =>
        if (v.pool.size < settings.keepPoolSize) {
          keys.map { key =>
            val proposition = key.publicImage
            val fee = factory.TestProps.txFee
            val timestamp = NetworkTime.time()
            val useBoxes = IndexedSeq(factory.genAssetBox(Address @@ key.publicImage.address)).map(_.id)
            val outputs = IndexedSeq((Address @@ factory.TestProps.recipientAddr, factory.TestProps.boxValue))
            val sig = PrivateKey25519Companion.sign(
              key,
              PaymentTransaction.getMessageToSign(proposition, fee, timestamp, useBoxes, outputs)
            )
            PaymentTransaction(proposition, fee, timestamp, sig, useBoxes, outputs)
          }
        } else {
          Seq()
        }
      }

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
