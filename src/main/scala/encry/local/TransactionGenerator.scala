package encry.local

import akka.actor.Actor
import encry.EncryApp._
import encry.account.Address
import encry.crypto.PrivateKey25519
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.mempool.{EncryTransaction, TransactionFactory}
import encry.modifiers.state.box.{AssetBox, EncryProposition}
import encry.network.EncryNodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import encry.settings.Algos
import encry.utils.Logging
import encry.utils.NetworkTime.Time
import encry.view.EncryNodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction

class TransactionGenerator extends Actor with Logging {

  import TransactionGenerator._

  var isActive: Boolean = false
  var limit: Int = settings.testing.limitPerEpoch
  var walletDataOpt: Option[WalletData] = None

  val noLimitMode: Boolean = settings.testing.limitPerEpoch < 0

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier[_]])
  }

  override def receive: Receive =
    handleTransactionGeneration orElse handleExternalEvents

  def handleTransactionGeneration: Receive = {

    case StartGeneration if !isActive =>
      log.info("Starting transaction generation")
      isActive = true

    case StopGeneration =>
      log.info("Stopping transaction generation")
      isActive = false

    case GenerateTransaction(walletData: WalletData) if isActive =>
      if (limit > 0) {
        (0 until limit).foldLeft(Seq[EncryTransaction](), walletData) {
          case ((txs, wd), i) =>
            if (wd.boxes.map(_.amount).sum >= (limit - i) * (amountD + minimalFeeD) ) {
              val tx: EncryTransaction = createTransaction(walletData)
              val leftBoxes: Seq[AssetBox] = wd.boxes.filterNot(bx => tx.inputs.map(_.boxId).contains(bx.id))
              (txs :+ tx) -> walletData.copy(boxes = leftBoxes)
            } else txs -> wd
        }._1.foreach(tx => {
          nodeViewHolder ! LocallyGeneratedTransaction[EncryProposition, EncryTransaction](tx)
        })
      }
  }

  def handleExternalEvents: Receive = {
    // Reset transaction limit counter and fetch latest wallet data
    case SemanticallySuccessfulModifier(_: EncryBlock) => nodeViewHolder ! FetchWalletData(settings.testing.limitPerEpoch, minimalFeeD)
  }

  def createTransaction(wd: WalletData): EncryTransaction = {
    val timestamp: Time = timeProvider.time()
    val boxes: IndexedSeq[AssetBox] = wd.boxes.foldLeft(Seq.empty[AssetBox]) { case (boxesAcc, box) =>
      if (boxesAcc.map(_.amount).sum < (amountD + minimalFeeD)) boxesAcc :+ box else boxesAcc
    }.toIndexedSeq
    TransactionFactory.defaultPaymentTransactionScratch(wd.secret, minimalFeeD, timestamp, boxes,
      Address @@ settings.testing.defaultRecipientAddress, amountD)
  }
}

object TransactionGenerator {

  val minimalFeeD: Int = 10000
  val amountD: Int = 100000

  case class WalletData(secret: PrivateKey25519, boxes: Seq[AssetBox])

  case class FetchWalletData(limit: Int, minimalFeeD: Int)

  case object StartGeneration

  case class GenerateTransaction(walletData: WalletData)

  case object StopGeneration
}
