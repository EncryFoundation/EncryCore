package encry.local

import akka.actor.{Actor, ActorRef, ActorRefFactory, Cancellable, Props}
import encry.account.Address
import encry.crypto.{PublicKey25519, Signature25519}
import encry.local.TransactionGenerator.{GeneratePaymentTransactions, StartGeneration, StopGeneration}
import encry.modifiers.mempool.{EncryBaseTransaction, PaymentTransaction}
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
import scorex.crypto.signatures.Curve25519

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
        Seq[PaymentTransaction]] { v =>
        if (v.pool.size < settings.keepPoolSize) {
          (0 until settings.keepPoolSize - v.pool.size).map { _ =>
            val pubKey = v.vault.publicKeys.head
            val recipient = pubKey.address
            val fee = 15L
            val amount: Long = Random.nextInt(30) + 9
            val accountPubKey = PublicKey25519(v.vault.keyManager.keys.head.publicImage.pubKeyBytes)
            val timestamp = timeProvider.time()
            if (v.vault.balance > 1000) {
              // Generate valid txs if vault's balance is enough.
              val boxes = v.vault.walletStorage.getAllBoxes.filter(_.isInstanceOf[AssetBox])
                .map(_.asInstanceOf[AssetBox]).foldLeft(Seq[AssetBox]()) {
                case (seq, box) => if (seq.map(_.amount).sum < (amount + fee)) seq :+ box else seq
              }
              val useBoxes = boxes.map(_.id).toIndexedSeq
              val outputs = IndexedSeq(
                (Address @@ recipient, amount),
                (Address @@ accountPubKey.address, boxes.map(_.amount).sum - (amount + fee)))
              val sig = Signature25519(Curve25519.sign(
                v.vault.keyManager.keys.head.privKeyBytes,
                PaymentTransaction.getMessageToSign(accountPubKey, fee, timestamp, useBoxes, outputs)
              ))

              PaymentTransaction(accountPubKey, fee, timestamp, sig, useBoxes, outputs)
            } else {
              // Generate semantically valid but stateful-invalid txs otherwise.
              val useBoxes = IndexedSeq(factory.genAssetBox(Address @@ pubKey.address)).map(_.id)
              val outputs = IndexedSeq((Address @@ factory.Props.recipientAddr, factory.Props.boxValue))
              val sig = Signature25519(Curve25519.sign(
                v.vault.keyManager.keys.head.privKeyBytes,
                PaymentTransaction.getMessageToSign(accountPubKey, fee, timestamp, useBoxes, outputs)
              ))
              PaymentTransaction(accountPubKey, fee, timestamp, sig, useBoxes, outputs)
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
