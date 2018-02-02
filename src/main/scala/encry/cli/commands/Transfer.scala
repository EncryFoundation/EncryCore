package encry.cli.commands

import akka.actor.ActorRef
import akka.pattern._
import akka.util.Timeout
import encry.account.Address
import encry.modifiers.mempool.{EncryBaseTransaction, PaymentTransaction}
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

import scala.util.Try

object Transfer extends Command {

  /**
    * Command "wallet -sendTx=toAddress;Amount"
    * Example "wallet -sendTx=3Y49ihvfesPcSfCxRLW4q4jjwzJhkFS8tFdN6KWMgcHSUvcngy;10"
    *
    * @param nodeViewHolderRef
    * @param args
    * @return
    */
  override def execute(nodeViewHolderRef: ActorRef, args: Array[String], settings: EncryAppSettings): Try[Unit] = Try {
    implicit val timeout: Timeout = Timeout(settings.scorexSettings.restApi.timeout)
    nodeViewHolderRef ?
      GetDataFromCurrentView[EncryHistory, UtxoState, EncryWallet, EncryMempool, Unit] { view =>
        val recipient = args(1).split(";").head
        val amount = args(1).split(";").last.toLong
        val proposition = view.vault.keyManager.keys.head.publicImage
        val fee = 100L
        val timestamp = 1234567L
        val boxes = view.vault.walletStorage.getAllBoxes.foldLeft(Seq[AssetBox]()) {
          case (seq, box) => if (seq.map(_.amount).sum < amount) seq :+ box else seq
        }
        val useBoxes = boxes.map(_.id).toIndexedSeq
        val outputs = IndexedSeq(
          (Address @@ recipient, amount),
          (Address @@ proposition.address, boxes.map(_.amount).sum - amount))
        val sig = Signature25519(Curve25519.sign(
          view.vault.keyManager.keys.head.privKeyBytes,
          PaymentTransaction.getMessageToSign(proposition, fee, timestamp, useBoxes, outputs)
        ))

        val tx = PaymentTransaction(proposition, fee, timestamp, sig, useBoxes, outputs)

        nodeViewHolderRef ! LocallyGeneratedTransaction[Proposition, EncryBaseTransaction](tx)
      }
  }
}
