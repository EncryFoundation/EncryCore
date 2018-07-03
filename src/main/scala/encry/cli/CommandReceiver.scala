package encry.cli

import encry.cli.commands._
import encry.crypto.PrivateKey25519
import encry.modifiers.mempool.{EncryTransaction, TransactionFactory}
import encry.modifiers.state.box.{AssetBox, EncryProposition}
import encry.settings.Algos
import encry.view.EncryNodeViewHolder
import encry.view.EncryNodeViewHolder.CurrentView
import encry.view.EncryNodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction
import encry.view.history.EncryHistory
import encry.view.state.EncryState

import scala.util.Try

trait CommandReceiver[StateType <: EncryState[StateType]] {
  this: EncryNodeViewHolder[StateType] =>
  def currentView: CurrentView[EncryHistory, StateType, VL, MP] = CurrentView(nodeView.history, nodeView.state, nodeView.wallet, nodeView.mempool)
  def commandReceive: Receive = {
    case AddKey =>
      if(Try(currentView.vault.keyManager.createNewKey()).isSuccess) Some(Response("OK"))
      else Some(Response("Operation failed"))
    case GetBalance =>
      val balances = currentView.vault.getBalances
      val res = if(balances.isEmpty) "<empty>"
      else balances.map(tokenInfo => s"TokenID(${Algos.encode(tokenInfo._1)}) : ${tokenInfo._2}\n").mkString("\n")
      sender() ! Some(Response(res))
    case PrintMyAddrs =>
      sender() ! Some(Response(currentView.vault.keyManager.keys.map(_.publicImage.address).mkString("\n")))
    case PrintPrivKeys =>
      sender() ! Some(Response(currentView.vault.keyManager.keys.map(k => Algos.encode(k.privKeyBytes)).mkString("\n")))
    case PrintPubKeys =>
      val res: String = Try(currentView.vault.keyManager.keys.map(_.publicKeyBytes).map(Algos.encode).mkString("\n")).getOrElse("ERROR!")
      sender() ! Some(Response(res))
    case Transfer.Request(recipient, fee, amount, timestamp) =>
      val view = currentView
      Try{
        val secret: PrivateKey25519 = view.vault.keyManager.keys.head
        val boxes: IndexedSeq[AssetBox] = view.vault.walletStorage.allBoxes.collect{case x: AssetBox => x}
          .foldLeft(0L -> Seq.empty[AssetBox]) { case ((sum, seq), box) =>
            if (sum < (amount + fee)) (sum + box.amount, seq :+ box) else (sum, seq)
          }._2.toIndexedSeq
        val tx: EncryTransaction = TransactionFactory.defaultPaymentTransactionScratch(secret, fee, timestamp, boxes, recipient, amount)
        self ! LocallyGeneratedTransaction[EncryProposition, EncryTransaction](tx)
        tx
      }.toOption.map(tx => Some(Response(tx.toString))).getOrElse(Some(Response("Operation failed. Malformed data.")))
  }
}
