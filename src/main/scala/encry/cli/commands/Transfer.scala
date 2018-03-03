package encry.cli.commands

import akka.actor.ActorRef
import akka.pattern._
import akka.util.Timeout
import encry.account.Address
import encry.cli.Response
import encry.modifiers.mempool.{EncryTransaction, TransactionFactory}
import encry.modifiers.state.box.AssetBox
import encry.settings.EncryAppSettings
import encry.view.history.EncryHistory
import encry.view.mempool.EncryMempool
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import scorex.core.LocalInterface.LocallyGeneratedTransaction
import scorex.core.NodeViewHolder.GetDataFromCurrentView
import scorex.core.transaction.box.proposition.Proposition

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

object Transfer extends Command {

  /**
    * Command "wallet -transfer=toAddress;Fee;Amount"
    * Example "wallet -transfer=31sVyJTL5nAaxogr1TBV7Y8tCDBwAaruGMQPAp1Qv7ZvSrjDHN;10;100"
    *
    * @param nodeViewHolderRef
    * @param args
    * @return
    */
  override def execute(nodeViewHolderRef: ActorRef,
                       args: Array[String], settings: EncryAppSettings): Option[Response] = {
    implicit val timeout: Timeout = Timeout(settings.scorexSettings.restApi.timeout)
    Await.result((nodeViewHolderRef ?
      GetDataFromCurrentView[EncryHistory, UtxoState, EncryWallet, EncryMempool, Option[Response]] { view =>
        Try {
          val secret = view.vault.keyManager.keys.head
          val recipient = Address @@ args(1).split(";").head
          val fee = args(1).split(";")(1).toLong
          val amount = args(1).split(";").last.toLong
          val timestamp = System.currentTimeMillis()  // TODO: Use NTP.
          val boxes = view.vault.walletStorage.allBoxes.filter(_.isInstanceOf[AssetBox]).map(_.asInstanceOf[AssetBox]).foldLeft(Seq[AssetBox]()) {
            case (seq, box) => if (seq.map(_.amount).sum < (amount + fee)) seq :+ box else seq
          }
          val useBoxes = boxes.map(_.id).toIndexedSeq

          val tx = TransactionFactory.defaultPaymentTransaction(secret, fee, timestamp, boxes, recipient, amount)

          nodeViewHolderRef ! LocallyGeneratedTransaction[Proposition, EncryTransaction](tx)

          tx
        }.toOption.map(tx => Some(Response(tx.toString))).getOrElse(Some(Response("Operation failed. Malformed data.")))
      }).mapTo[Option[Response]], 5.second)
  }
}
