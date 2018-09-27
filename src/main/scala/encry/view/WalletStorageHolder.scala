package encry.view

import akka.actor.Actor
import scala.concurrent.duration._
import encry.EncryApp.{settings, system}
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

  system.scheduler.schedule(settings.walletStorageHolder.map(_.startTime.second).getOrElse(10 second),
    settings.walletStorageHolder.map(_.askTime.second).getOrElse(15 second)) {
    system.actorSelection("user/nodeViewHolder") !
      GetDataFromCurrentView[EncryHistory, UtxoState, EncryWallet, EncryMempool, Seq[EncryBaseBox]] {
        _.vault.walletStorage.allBoxes
      }
  }

  override def receive: Receive =  changeCollection()

  def changeCollection(boxes: Seq[EncryBaseBox] = Seq()): Receive = {
    case GetAllBoxes() =>
      val boxesForSend: Seq[EncryBaseBox] = boxes.takeRight(settings.walletStorageHolder.map(_.boxesQty).getOrElse(500))
      sender() ! boxesForSend
      context.become(changeCollection(boxes.diff(boxesForSend)))
    case seq: Seq[EncryBaseBox] =>
      if (settings.influxDB.isDefined)
        system.actorSelection("user/statsSender") ! CurrentUtxosQtyInIOdb(seq.size)
      context.become(changeCollection(seq))
    case _ =>
  }
}