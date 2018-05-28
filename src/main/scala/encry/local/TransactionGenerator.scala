package encry.local

import akka.actor.Actor
import encry.EncryApp._
import encry.account.Address
import encry.crypto.PrivateKey25519
import encry.local.TransactionGenerator.{GenerateTransactions, StartGeneration, StopGeneration}
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.mempool.{EncryTransaction, TransactionFactory}
import encry.modifiers.state.box.proposition.EncryProposition
import encry.modifiers.state.box.{AssetBox, EncryBaseBox}
import encry.utils.BalanceCalculator
import encry.view.history.EncryHistory
import encry.view.mempool.EncryMempool
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import scorex.core.NodeViewHolder.ReceivableMessages.{GetDataFromCurrentView, LocallyGeneratedTransaction}
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import scorex.core.transaction.box.Box.Amount
import scorex.core.utils.NetworkTime.Time
import scorex.core.utils.ScorexLogging

import scala.concurrent.duration._

class TransactionGenerator extends Actor with ScorexLogging {

  var isActive: Boolean = false
  var usedBoxes: List[EncryBaseBox] = List.empty

  // TODO: Read from config.
  val minimalFee = 10000
  val amount = 100

  override def preStart(): Unit = context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier[_]])

  // TODO: Limit tracking.
  override def receive: Receive = {

    case StartGeneration if !isActive =>
      log.info("Starting transaction generation")
      isActive = true
      context.system.scheduler.scheduleOnce(1500.millis)(self ! GenerateTransactions)

    case GenerateTransactions =>
      generateTransactions()

    case transactions: List[EncryTransaction] =>
      transactions.foreach(tx => nodeViewHolder ! LocallyGeneratedTransaction[EncryProposition, EncryTransaction](tx))
      if (isActive) self ! GenerateTransactions

    case StopGeneration =>
      isActive = false

    case SemanticallySuccessfulModifier(mod: EncryBlock) =>
      usedBoxes = List.empty
  }

  def generateTransactions(): Unit =
    nodeViewHolder ! GetDataFromCurrentView[EncryHistory, UtxoState, EncryWallet, EncryMempool, List[EncryTransaction]] { v =>
      val wallet: EncryWallet = v.vault
      val balance: Time = wallet.getBalances.find(_._1 sameElements BalanceCalculator.intrinsicTokenId).map(_._2).getOrElse(0L)
      val availableBoxes: Seq[AssetBox] = wallet.walletStorage.allBoxes.foldLeft(Seq.empty[AssetBox]) {
        case (acc, box: AssetBox) => acc :+ box
        case (acc, _) => acc
      }
      val qty: Long =
        if (encrySettings.testingSettings.epochLimit <= 0) balance / (minimalFee + amount)
        else encrySettings.testingSettings.epochLimit
      (0L to qty).foldLeft(List.empty[EncryTransaction]) {
        case (acc, _) if availableBoxes.map(_.amount).sum >= minimalFee + amount =>
          acc :+ assemblyTransaction(wallet.keyManager.mainKey, availableBoxes, minimalFee, amount)
        case (acc, _) => acc
      }
    }

  def assemblyTransaction(secret: PrivateKey25519,
                          availableBoxes: Seq[AssetBox],
                          fee: Amount,
                          amount: Amount): EncryTransaction = {
    val timestamp: Time = timeProvider.time()
    val boxes: IndexedSeq[AssetBox] = availableBoxes.foldLeft(Seq.empty[AssetBox]) {
      case (acc, box) if !usedBoxes.contains(box) && acc.map(_.amount).sum < (amount + fee) => acc :+ box
      case (acc, _) => acc
    }.toIndexedSeq
    usedBoxes = usedBoxes ++ boxes
    TransactionFactory.defaultPaymentTransactionScratch(secret, fee, timestamp, boxes,
      Address @@ encrySettings.testingSettings.defaultRecipientAddress, amount)
  }
}

object TransactionGenerator {

  case object StartGeneration

  case object GenerateTransactions

  case object StopGeneration
}
