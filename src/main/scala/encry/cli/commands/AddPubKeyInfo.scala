package encry.cli.commands

import akka.actor.ActorRef
import akka.pattern._
import akka.util.Timeout
import encry.cli.Response
import encry.common.KeyPairType
import encry.modifiers.mempool.{AddPubKeyInfoTransaction, AddPubKeyInfoTransactionSerializer}
import encry.modifiers.state.box.AssetBox
import encry.settings.{Algos, EncryAppSettings}
import encry.view.history.EncryHistory
import encry.view.mempool.EncryMempool
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import scorex.core.LocalInterface.LocallyGeneratedTransaction
import scorex.core.NodeViewHolder.GetDataFromCurrentView
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.crypto.signatures.{Curve25519, PublicKey, Signature}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

object AddPubKeyInfo extends Command {

  /**
    * Command "pki -addPubKeyInfo=PubKeyEncoded;PubKeyProofEncoded;PubKeyInfoEncoded;PubKeyTypeName;Fee"
    * Example "pki -addPubKeyInfo=3Y49ihvfesPcSfCxRLW4q4jjwzJhkFS8tFdN6KWMgcHSUvcngy;5vLvsiu7y14BCeVA68GyRpkUY29Dp8gdhqSbcsfJtc3n4foG1vs2xPtJCdMz37pb65WyYUoCLJVQvirzAt5t6CHj;3Y49ihvfesPcSfCxRLW4q4jjwzJhkFS8tFdN6KWMgcHSUvcngy;Pair25519;39"
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
          val cArgs = args(1).split(";")
          val pubKeyBytes = PublicKey @@ Algos.decode(cArgs.head).get
          val pubKeyProofBytes = Signature @@ Algos.decode(cArgs(1)).get
          val pubKeyInfoBytes = Algos.decode(cArgs(2)).get
          val pubKeyTypeId = KeyPairType.pairTypeByName(cArgs(3)).typeId
          val fee = cArgs(4).toLong
          val proposition = view.vault.keyManager.keys.head.publicImage
          val timestamp = System.currentTimeMillis() // TODO: Use NTP.
          val boxes = view.vault.walletStorage.getAllBoxes.foldLeft(Seq[AssetBox]()) {
            case (seq, box) => if (seq.map(_.amount).sum < fee) seq :+ box else seq
          }
          val useBoxes = boxes.map(_.id).toIndexedSeq
          val change = boxes.map(_.amount).sum - fee
          val sig = Signature25519(Curve25519.sign(
            view.vault.keyManager.keys.head.privKeyBytes,
            AddPubKeyInfoTransaction.getMessageToSign(proposition, fee, timestamp,
              useBoxes, change, pubKeyBytes, pubKeyProofBytes, pubKeyInfoBytes, pubKeyTypeId)
          ))

          val tx = AddPubKeyInfoTransaction(proposition, fee, timestamp, sig,
            useBoxes, change, pubKeyBytes, pubKeyProofBytes, pubKeyInfoBytes, pubKeyTypeId)

          val txDeserTry = AddPubKeyInfoTransactionSerializer.parseBytes(tx.bytes)

          assert(txDeserTry.isSuccess, "Cannot deserialize tx")
          assert(tx.id sameElements txDeserTry.get.id, "Deserialization failed")

          nodeViewHolderRef ! LocallyGeneratedTransaction[Proposition, AddPubKeyInfoTransaction](tx)

          tx
        }.toOption.map(tx => Some(Response(tx.toString))).getOrElse(Some(Response("Operation failed. Malformed data.")))
      }).mapTo[Option[Response]], 5.second)
  }
}
