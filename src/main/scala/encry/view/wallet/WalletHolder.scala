package encry.view.wallet

import akka.persistence.PersistentActor
import com.google.common.primitives.Longs
import encry.modifiers.state.box.EncryBaseBox
import encry.modifiers.state.box.TokenIssuingBox.TokenId
import encry.utils.Logging
import encry.view.wallet.WalletHolder.{GetCurrentBalance, UpdateCurrentState}
import akka.util.ByteString
import org.encryfoundation.common.Algos
import scorex.crypto.hash.Digest32

class WalletHolder extends PersistentActor with Logging {

  val a: Digest32 = Algos.hash("balances")

  override def receiveRecover: Receive = changeState

  override def receiveCommand: Receive = changeState

  def changeState(currentBoxesState: Map[TokenId, EncryBaseBox] = Map(),
                  currentBalance: ByteString = ByteString.empty): Receive = {
    case UpdateCurrentState(toRemove, toUpdate) =>

    case GetCurrentBalance => currentBalance.sliding(40, 40)
      .map(ch => ch.take(32) -> Longs.fromByteArray(currentBalance.toArray.takeRight(8))).map(._toMap).getOrElse(Map.empty)

  }

  override def persistenceId: String = "wallet persistent actor"

  override def journalPluginId: String = "akka.persistence.journal.leveldb"
}

object  WalletHolder {

  case class UpdateCurrentState(toRemove: Seq[EncryBaseBox], toUpdate: Seq[EncryBaseBox])

  case class GetCurrentBalance()

}