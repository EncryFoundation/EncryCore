package encry.view

import akka.actor.Actor
import scala.concurrent.duration._
import encry.EncryApp.{settings, system}
import encry.api.http.routes.GetAllBoxes
import encry.modifiers.state.box.EncryBaseBox
import encry.stats.StatsSender.CurrentUtxosQtyInIOdb
import encry.view.history.EncryHistory
import encry.view.mempool.Mempool
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import scala.concurrent.ExecutionContext.Implicits.global
import encry.view.EncryNodeViewHolder.ReceivableMessages.GetDataFromCurrentView

class WalletStorageHolder extends Actor {

  val txsQty: Int = settings.walletStorageHolder.map(_.boxesQty).getOrElse(500)

  system.scheduler.schedule(settings.walletStorageHolder.map(_.startTime.second).getOrElse(10 second),
    settings.walletStorageHolder.map(_.askTime.second).getOrElse(15 second)) {
    system.actorSelection("user/nodeViewHolder") !
      GetDataFromCurrentView[EncryHistory, UtxoState, EncryWallet, Mempool, Seq[EncryBaseBox]] {
        _.vault.walletStorage.allBoxes
      }
  }

  override def receive: Receive =  changeCollection()

  def changeCollection(boxes: Seq[EncryBaseBox] = Seq()): Receive = {
    case GetAllBoxes() =>
      sender() ! boxes.takeRight(txsQty)
      context.become(changeCollection(boxes.dropRight(txsQty)))
    case seq: Seq[EncryBaseBox] =>
      if (settings.influxDB.isDefined)
        system.actorSelection("user/statsSender") ! CurrentUtxosQtyInIOdb(seq.size)
      context.become(changeCollection(seq))
    case _ =>
  }
}