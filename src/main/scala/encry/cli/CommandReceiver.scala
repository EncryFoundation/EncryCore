package encry.cli

import encry.cli.Commands._
import encry.crypto.PrivateKey25519
import encry.modifiers.mempool.{EncryTransaction, TransactionFactory}
import encry.modifiers.state.box.{AssetBox, EncryProposition}
import encry.settings.Algos
import encry.utils.Mnemonic
import encry.view.EncryNodeViewHolder
import encry.view.EncryNodeViewHolder.CurrentView
import encry.view.EncryNodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction
import encry.view.history.EncryHistory
import encry.view.state.EncryState

import scala.util.Try
import encry.utils.ExtUtils._
import encry.view.mempool.EncryMempool
import encry.view.wallet.EncryWallet

trait CommandReceiver[StateType <: EncryState[StateType]] {
  this: EncryNodeViewHolder[StateType] =>
  def currentView: CurrentView[EncryHistory, StateType, EncryWallet, EncryMempool] =
    CurrentView(nodeView.history, nodeView.state, nodeView.wallet, nodeView.mempool)
  def keys: Seq[PrivateKey25519] = currentView.vault.keyManager.keys
  def send(s: String): Unit = sender() ! Some(Response(s))
  def commandReceive: Receive = {
    case AddKey => send(if (Try(currentView.vault.keyManager.createNewKey()).isSuccess) "New key added." else "Operation failed")
    case GetBalance =>
      val balances = currentView.vault.getBalances.map(tokenInfo => s"TokenID(${Algos.encode(tokenInfo._1)}) : ${tokenInfo._2}\n")
      send(if(balances.isEmpty) "<empty>" else balances.mkString("\n"))
    case PrintMyAddrs => send(keys.map(_.publicImage.address).mkString("\n"))
    case PrintPrivKeys => send(keys.map(k => Algos.encode(k.privKeyBytes)).mkString("\n"))
    case PrintPubKeys => send(Try(keys.map(_.publicKeyBytes).map(Algos.encode).mkString("\n")).getOrElse("ERROR!"))
    case InitKeyStorage.Request(mnemonicCode) =>
      Try(currentView.vault.keyManager.initStorage(Mnemonic.mnemonicCodeToBytes(mnemonicCode)))
        .map(_ => s"Your mnemonic code is: $mnemonicCode")
        .getOrElse("Operation failed. Couldn't init key storage.")
        .iapply(send)
    case Transfer.Request(recipient, fee, amount, timestamp) =>
      Try {
        currentView.vault.walletStorage.allBoxes.collect { case x: AssetBox => x }
          .foldLeft(0L -> Seq.empty[AssetBox]) { case ((sum, seq), box) =>
            if (sum < (amount + fee)) (sum + box.amount, seq :+ box) else sum -> seq
          }._2.toIndexedSeq
      } .map(TransactionFactory.defaultPaymentTransactionScratch(keys.head, fee, timestamp, _, recipient, amount))
        .map(_.iapply(self ! LocallyGeneratedTransaction[EncryProposition, EncryTransaction](_)))
        .map(_.toString).toOption.getOrElse("Operation failed. Malformed data.")
        .iapply(send)
  }
}
