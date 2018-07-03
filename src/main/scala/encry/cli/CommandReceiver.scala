package encry.cli

import encry.cli.commands._
import encry.settings.Algos
import encry.view.EncryNodeViewHolder
import encry.view.EncryNodeViewHolder.CurrentView
import encry.view.state.EncryState

import scala.util.Try

trait CommandReceiver[StateType <: EncryState[StateType]] {
  this: EncryNodeViewHolder[StateType] =>
  def currentView: CurrentView[HIS, StateType, VL, MP] = CurrentView(nodeView.history, nodeView.state, nodeView.wallet, nodeView.mempool)
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
  }
}
