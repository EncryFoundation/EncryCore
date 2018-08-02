package encry.local

import akka.actor.Actor
import encry.EncryApp._
import encry.Address
import encry.crypto.PrivateKey25519
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.mempool.{EncryTransaction, TransactionFactory}
import encry.modifiers.state.box.{AssetBox, EncryProposition}
import encry.network.EncryNodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import encry.stats.StatsSender.TransactionGeneratorStat
import encry.utils.Logging
import encry.view.EncryNodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction

class TransactionGenerator extends Actor with Logging {

  import TransactionGenerator._

  val limit: Int = settings.testing.limitPerEpoch

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier[_]])
  }

  def receive: Receive = {
    case GenerateTransaction(walletData: WalletData) if limit > 0 =>
      val startTime: Long = System.currentTimeMillis()
      val txs: Seq[EncryTransaction] = (0 until limit).foldLeft(Seq[EncryTransaction](), walletData) {
        case ((transactions, wd), i) =>
          if (wd.boxes.map(_.amount).sum > (limit - i) * (amountD + minimalFeeD)) {
            val tx: EncryTransaction = createTransaction(wd)
            val leftBoxes: Seq[AssetBox] = wd.boxes.filterNot(bx => tx.inputs.map(_.boxId).contains(bx.id))
            (transactions :+ tx) -> wd.copy(boxes = leftBoxes)
          } else transactions -> wd
      }._1
      if (settings.node.sendStat)
        context.system.actorSelection("user/statsSender") ! TransactionGeneratorStat(txs.size, System.currentTimeMillis() - startTime)
      txs.foreach(tx =>
        nodeViewHolder ! LocallyGeneratedTransaction[EncryProposition, EncryTransaction](tx)
      )
    case SemanticallySuccessfulModifier(_: EncryBlock) => nodeViewHolder ! FetchWalletData(settings.testing.limitPerEpoch, minimalFeeD)
  }

  def createTransaction(wd: WalletData): EncryTransaction = {
    val boxes: IndexedSeq[AssetBox] = wd.boxes.foldLeft(Seq.empty[AssetBox]) { case (boxesAcc, box) =>
      if (boxesAcc.map(_.amount).sum <= (amountD + minimalFeeD)) boxesAcc :+ box else boxesAcc
    }.toIndexedSeq
    TransactionFactory.defaultPaymentTransactionScratch(wd.secret, minimalFeeD, timeProvider.time(), boxes,
      Address @@ settings.testing.defaultRecipientAddress, amountD)
  }
}

object TransactionGenerator {

  val minimalFeeD: Int = settings.testing.minimalFee
  val amountD: Int = settings.testing.amount

  case class WalletData(secret: PrivateKey25519, boxes: Seq[AssetBox])

  case class FetchWalletData(limit: Int, minimalFeeD: Int)

  case object StartGeneration

  case class GenerateTransaction(walletData: WalletData)

}
