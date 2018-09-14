package encry.view

import akka.actor.Actor
import scala.concurrent.duration._
import encry.EncryApp.system
import encry.api.http.routes.GetAllBoxes
import encry.modifiers.state.box.EncryBaseBox
import encry.view.history.EncryHistory
import encry.view.mempool.EncryMempool
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import scala.concurrent.ExecutionContext.Implicits.global
import encry.view.EncryNodeViewHolder.ReceivableMessages.GetDataFromCurrentView

class WalletStorageHolder extends Actor {

  var buffer = Set.empty[EncryBaseBox]

  system.scheduler.schedule(0 second, 15 second) {
    system.actorSelection("user/nodeViewHolder") !
      GetDataFromCurrentView[EncryHistory, UtxoState, EncryWallet, EncryMempool, Seq[EncryBaseBox]] {
        _.vault.walletStorage.allBoxes }
  }

  system.scheduler.schedule(5 second, 15 second) {
    println(buffer.size)
  }

  override def receive: Receive = {
    case GetAllBoxes() => sender() ! buffer.toSeq
    case seq: Seq[EncryBaseBox] => buffer ++= seq
  }
}