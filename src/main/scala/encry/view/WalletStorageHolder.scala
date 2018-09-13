package encry.view

import akka.actor.Actor
import scala.concurrent.duration._
import encry.EncryApp.system
import scala.concurrent.ExecutionContext.Implicits.global
import encry.view.wallet.WalletStorage._

class WalletStorageHolder extends Actor {



  system.scheduler.schedule(5 second, 5 second) {

  }

  override def receive: Receive = {
    case _ =>
  }
}
