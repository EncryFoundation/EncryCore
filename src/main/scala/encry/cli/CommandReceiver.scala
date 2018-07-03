package encry.cli

import encry.cli.commands._
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
trait CommandReceiver[StateType <: EncryState[StateType]] {
  this: EncryNodeViewHolder[StateType] =>
  def currentView: CurrentView[EncryHistory, StateType, VL, MP] = CurrentView(nodeView.history, nodeView.state, nodeView.wallet, nodeView.mempool)
  def keys: Seq[PrivateKey25519] = currentView.vault.keyManager.keys
  def response(s: String): Option[Response] = Some(Response(s))
  def commandReceive: Receive = {
    case AddKey =>
      if(Try(currentView.vault.keyManager.createNewKey()).isSuccess) Some(Response("OK"))
      else Some(Response("Operation failed"))
    case GetBalance =>
      val balances = currentView.vault.getBalances
      val res = if(balances.isEmpty) "<empty>"
      else balances.map(tokenInfo => s"TokenID(${Algos.encode(tokenInfo._1)}) : ${tokenInfo._2}\n").mkString("\n")
      sender() ! response(res)
    case PrintMyAddrs =>
      sender() ! response(keys.map(_.publicImage.address).mkString("\n"))
    case PrintPrivKeys =>
      sender() ! response(keys.map(k => Algos.encode(k.privKeyBytes)).mkString("\n"))
    case PrintPubKeys =>
      val res: String = Try(keys.map(_.publicKeyBytes).map(Algos.encode).mkString("\n")).getOrElse("ERROR!")
      sender() ! response(res)
    case InitKeyStorage.Request(mnemonicCode) =>
      sender() ! Try(currentView.vault.keyManager.initStorage(Mnemonic.mnemonicCodeToBytes(mnemonicCode)))
        .map(_ => mnemonicCode)
        .map(code => response(s"Your mnemonic code is: $code"))
        .getOrElse(response("Operation failed. Couldn't init key storage."))
    case Transfer.Request(recipient, fee, amount, timestamp) =>
      val view = currentView
      sender() ! Try{
        val boxes: IndexedSeq[AssetBox] = view.vault.walletStorage.allBoxes.collect{case x: AssetBox => x}
          .foldLeft(0L -> Seq.empty[AssetBox]) { case ((sum, seq), box) =>
            if (sum < (amount + fee)) (sum + box.amount, seq :+ box) else (sum, seq)
          }._2.toIndexedSeq
        TransactionFactory.defaultPaymentTransactionScratch(keys.head, fee, timestamp, boxes, recipient, amount).iapply(
          self ! LocallyGeneratedTransaction[EncryProposition, EncryTransaction](_)
        )
      }.toOption.map(tx => response(tx.toString)).getOrElse(response("Operation failed. Malformed data."))

  }
}
