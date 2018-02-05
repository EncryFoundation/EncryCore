package encry.cli.commands

import akka.actor.ActorRef
import akka.pattern._
import akka.util.Timeout
import encry.cli.Response
import encry.modifiers.mempool.{AddPubKeyInfoTransaction, AddPubKeyInfoTransactionSerializer}
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
import scorex.crypto.signatures.{Curve25519, PublicKey, Signature}
import scorex.utils.Random

import scala.util.Try

object AddPubKeyInfo extends Command {

  /**
    * Command "pki -addPubKeyInfo=PubKeyEncoded;PubKeyProofEncoded;PubKeyInfo;Fee"
    * Example "pki -addPubKeyInfo=3Y49ihvfesPcSfCxRLW4q4jjwzJhkFS8tFdN6KWMgcHSUvcngy;5vLvsiu7y14BCeVA68GyRpkUY29Dp8gdhqSbcsfJtc3n4foG1vs2xPtJCdMz37pb65WyYUoCLJVQvirzAt5t6CHj;KeyInfo;39"
    *
    * @param nodeViewHolderRef
    * @param args
    * @return
    */
  // TODO: Input validation.
  override def execute(nodeViewHolderRef: ActorRef,
                       args: Array[String], settings: EncryAppSettings): Option[Response] = Try {
    implicit val timeout: Timeout = Timeout(settings.scorexSettings.restApi.timeout)
    nodeViewHolderRef ?
      GetDataFromCurrentView[EncryHistory, UtxoState, EncryWallet, EncryMempool, Unit] { view =>
        // FIXME: UTF-8 String causes persistent prover crashing.
        val pubKeyBytes = PublicKey @@ Random.randomBytes()
        val pubKeyProofBytes = Signature @@ Random.randomBytes(64)
        val pubKeyInfo = ""
        val fee = 20L
        val proposition = view.vault.keyManager.keys.head.publicImage
        val timestamp = System.currentTimeMillis()  // TODO: Use NTP.
        val boxes = view.vault.walletStorage.getAllBoxes.foldLeft(Seq[AssetBox]()) {
          case (seq, box) => if (seq.map(_.amount).sum < fee) seq :+ box else seq
        }
        val useBoxes = boxes.map(_.id).toIndexedSeq
        val change = boxes.map(_.amount).sum - fee
        val sig = Signature25519(Curve25519.sign(
          view.vault.keyManager.keys.head.privKeyBytes,
          AddPubKeyInfoTransaction.getMessageToSign(
            proposition, fee, timestamp, useBoxes, change, pubKeyBytes, pubKeyProofBytes, pubKeyInfo)
        ))

        val tx = AddPubKeyInfoTransaction(
          proposition, fee, timestamp, sig, useBoxes, change, pubKeyBytes, pubKeyProofBytes, pubKeyInfo)

        val txDeserTry = AddPubKeyInfoTransactionSerializer.parseBytes(tx.bytes)

        assert(txDeserTry.isSuccess, "Cannot deserialize tx")
        assert(tx.id sameElements txDeserTry.get.id, "Deserialization failed")

        println(tx.json)

        nodeViewHolderRef ! LocallyGeneratedTransaction[Proposition, AddPubKeyInfoTransaction](tx)
      }
  }.toOption.map(_ => Some(Response("OK"))).getOrElse(Some(Response("Operation failed")))
}
