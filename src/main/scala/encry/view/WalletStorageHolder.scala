package encry.view

import akka.actor.Actor
import scala.concurrent.duration._
import encry.EncryApp.system
import encry.api.http.routes.GetAllBoxes
import encry.modifiers.state.box.EncryBaseBox
import encry.stats.StatsSender.CurrentUtxosQtyInIOdb
import encry.view.history.EncryHistory
import encry.view.mempool.EncryMempool
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import scala.concurrent.ExecutionContext.Implicits.global
import encry.view.EncryNodeViewHolder.ReceivableMessages.GetDataFromCurrentView

class WalletStorageHolder extends Actor {

  var buffer = Seq.empty[EncryBaseBox]

  system.scheduler.schedule(10 second, 60 second) {
    system.actorSelection("user/nodeViewHolder") !
      GetDataFromCurrentView[EncryHistory, UtxoState, EncryWallet, EncryMempool, Seq[EncryBaseBox]] {
        _.vault.walletStorage.allBoxes }
  }

  override def receive: Receive = {
    case GetAllBoxes() => sender() ! buffer.takeRight(500)
      buffer = buffer.dropRight(500)
    case seq: Seq[EncryBaseBox] => buffer = seq
      system.actorSelection("user/statsSender") ! CurrentUtxosQtyInIOdb(seq.size)
  }
}