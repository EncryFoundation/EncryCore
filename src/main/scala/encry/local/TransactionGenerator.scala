package encry.local

import akka.actor.Actor
import encry.EncryApp._
import encry.account.Address
import encry.crypto.PrivateKey25519
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.mempool.{EncryTransaction, TransactionFactory}
import encry.modifiers.state.box.AssetBox
import encry.modifiers.state.box.proposition.EncryProposition
import encry.view.history.EncryHistory
import encry.view.mempool.EncryMempool
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import scorex.core.NodeViewHolder.ReceivableMessages.{GetDataFromCurrentView, LocallyGeneratedTransaction}
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{ChangedVault, SemanticallySuccessfulModifier}
import scorex.core.utils.NetworkTime.Time
import scorex.core.utils.ScorexLogging

import scala.concurrent.duration._

class TransactionGenerator extends Actor with ScorexLogging {

  import TransactionGenerator._

  var isActive: Boolean = false
  var limit: Int = encrySettings.testingSettings.epochLimit
  var walletDataOpt: Option[WalletData] = None

  val noLimitMode: Boolean = encrySettings.testingSettings.epochLimit < 0

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[ChangedVault])
    context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier[_]])
  }

  override def receive: Receive =
    handleTransactionGeneration orElse
      handleWalletData orElse
      handleExternalEvents

  def handleTransactionGeneration: Receive = {

    case StartGeneration if !isActive =>
      log.info("Starting transaction generation")
      isActive = true
      context.system.scheduler.scheduleOnce(500.millis)(self ! FetchWalletData)
      context.system.scheduler.scheduleOnce(1500.millis)(self ! GenerateTransaction)

    case StopGeneration =>
      log.info("Stopping transaction generation")
      isActive = false

    case GenerateTransaction if isActive =>
      walletDataOpt match {
        // Generate new transaction if wallet contains enough coins and transaction limit is not exhausted.
        case Some(walletData) if walletData.boxes.map(_.amount).sum >= (amountD + minimalFeeD) && (limit > 0 || noLimitMode) =>
          val tx: EncryTransaction = createTransaction(walletData)
          val leftBoxes: Seq[AssetBox] = walletData.boxes.filterNot(bx => tx.unlockers.map(_.boxId).contains(bx.id))
          walletDataOpt = Some(walletData.copy(boxes = leftBoxes))
          limit -= 1
          nodeViewHolder ! LocallyGeneratedTransaction[EncryProposition, EncryTransaction](tx)
          self ! GenerateTransaction
        // Retry in 5 sec otherwise
        case _ =>
          context.system.scheduler.scheduleOnce(5000.millis)(self ! GenerateTransaction)
      }
  }

  def handleWalletData: Receive = {
    case FetchWalletData => fetchWalletData()
    case wd: WalletData => walletDataOpt = Some(wd)
  }

  def handleExternalEvents: Receive = {
    // Reset transaction limit counter and fetch latest wallet data
    case SemanticallySuccessfulModifier(_: EncryBlock) =>
      self ! FetchWalletData
      limit = encrySettings.testingSettings.epochLimit
  }

  def fetchWalletData(): Unit =
    nodeViewHolder ! GetDataFromCurrentView[EncryHistory, UtxoState, EncryWallet, EncryMempool, WalletData] { v =>
      val wallet: EncryWallet = v.vault
      val availableBoxes: Seq[AssetBox] = wallet.walletStorage.allBoxes.foldLeft(Seq.empty[AssetBox]) {
        case (acc, box: AssetBox) if box.isIntrinsic => acc :+ box
        case (acc, _) => acc
      }
      WalletData(wallet.keyManager.mainKey, availableBoxes)
    }

  def createTransaction(wd: WalletData): EncryTransaction = {
    val timestamp: Time = timeProvider.time()
    val boxes: IndexedSeq[AssetBox] = wd.boxes.foldLeft(Seq.empty[AssetBox]) { case (boxesAcc, box) =>
      if (boxesAcc.map(_.amount).sum < (amountD + minimalFeeD)) boxesAcc :+ box else boxesAcc
    }.toIndexedSeq
    TransactionFactory.defaultPaymentTransactionScratch(wd.secret, minimalFeeD, timestamp, boxes,
      Address @@ encrySettings.testingSettings.defaultRecipientAddress, amountD)
  }
}

object TransactionGenerator {

  val minimalFeeD = 10000
  val amountD = 100

  case class WalletData(secret: PrivateKey25519, boxes: Seq[AssetBox])

  case object FetchWalletData

  case object StartGeneration

  case object GenerateTransaction

  case object StopGeneration
}
