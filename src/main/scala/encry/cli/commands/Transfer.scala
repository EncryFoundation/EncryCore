package encry.cli.commands

import akka.actor.ActorRef
import akka.pattern._
import akka.util.Timeout
import encry.account.Address
import encry.cli.Response
import encry.modifiers.mempool.PaymentTransaction
import encry.modifiers.state.box.AssetBox
import encry.settings.EncryAppSettings
import encry.view.history.EncryHistory
import encry.view.mempool.EncryMempool
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import scorex.core.LocalInterface.LocallyGeneratedTransaction
import scorex.core.NodeViewHolder.GetDataFromCurrentView
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.crypto.signatures.Curve25519

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

object Transfer extends Command {

  /**
    * Command "wallet -transfer=toAddress;Fee;Amount"
    * Example "wallet -transfer=3Y49ihvfesPcSfCxRLW4q4jjwzJhkFS8tFdN6KWMgcHSUvcngy;10;100"
    *
    * @param nodeViewHolderRef
    * @param args
    * @return
    */
  // TODO: Input validation.
  override def execute(nodeViewHolderRef: ActorRef,
                       args: Array[String], settings: EncryAppSettings): Option[Response] = {
    implicit val timeout: Timeout = Timeout(settings.scorexSettings.restApi.timeout)
    Await.result((nodeViewHolderRef ?
      GetDataFromCurrentView[EncryHistory, UtxoState, EncryWallet, EncryMempool, Option[Response]] { view =>
        Try {
          val recipient = args(1).split(";").head
          val fee = args(1).split(";")(1).toLong
          val amount = args(1).split(";").last.toLong
          val proposition = view.vault.keyManager.keys.head.publicImage
          val timestamp = System.currentTimeMillis()  // TODO: Use NTP.
          val boxes = view.vault.walletStorage.getAllBoxes.foldLeft(Seq[AssetBox]()) {
            case (seq, box) => if (seq.map(_.amount).sum < (amount + fee)) seq :+ box else seq
          }
          val useBoxes = boxes.map(_.id).toIndexedSeq
          val outputs = IndexedSeq(
            (Address @@ recipient, amount),
            (Address @@ proposition.address, boxes.map(_.amount).sum - (amount + fee)))
          val sig = Signature25519(Curve25519.sign(
            view.vault.keyManager.keys.head.privKeyBytes,
            PaymentTransaction.getMessageToSign(proposition, fee, timestamp, useBoxes, outputs)
          ))

          val tx = PaymentTransaction(proposition, fee, timestamp, sig, useBoxes, outputs)

          nodeViewHolderRef ! LocallyGeneratedTransaction[Proposition, PaymentTransaction](tx)

          tx
        }.toOption.map(tx => Some(Response(tx.toString))).getOrElse(Some(Response("Operation failed. Malformed data.")))
      }).mapTo[Option[Response]], 5.second)
  }
}
