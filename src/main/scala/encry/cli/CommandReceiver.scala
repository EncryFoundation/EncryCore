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
    case "PING" =>
      println("Received PING")
      sender() ! "PONG"
    case AddKey =>
      if(Try(currentView.vault.keyManager.createNewKey()).isSuccess) Some(Response("OK"))
      else Some(Response("Operation failed"))
    case GetBalance =>
      val r = currentView.vault.getBalances.foldLeft("")((str, tokenInfo) =>
        str.concat(s"TokenID(${Algos.encode(tokenInfo._1)}) : ${tokenInfo._2}\n"))
      val res = if (r.length == 0) "<empty>" else r
      sender() ! Some(Response(res))
    case InitKeyStorage =>
    //      Try {
    //        val mnemonicCode = args.requireArgOrElse("seed", Ast.Str(Mnemonic.entropyToMnemonicCode(SecureRandom.getSeed(16)))).s
    //        currentView.vault.keyManager.initStorage(Mnemonic.mnemonicCodeToBytes(mnemonicCode))
    //        mnemonicCode
    //      }.toOption.map(code => Some(Response(s"Your mnemonic code is: $code"))).getOrElse(Some(Response("Operation failed. Couldn't init key storage.")))

    case PrintMyAddrs =>
      sender() ! Some(Response(currentView.vault.keyManager.keys.foldLeft("")((str, k) => str + k.publicImage.address + "\n")))
    case PrintPrivKeys =>
      sender() ! Some(Response(currentView.vault.keyManager.keys.foldLeft("")((str, k) => str + Algos.encode(k.privKeyBytes)) + "\n"))
    case PrintPubKeys =>
      val res: String = Try(currentView.vault.keyManager.keys.map(_.publicKeyBytes).map(Algos.encode).mkString("\n")).getOrElse("ERROR!")
      sender() ! Some(Response(res))
  }
}
