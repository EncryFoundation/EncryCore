package encry.view.wallet

import akka.persistence.PersistentActor
import com.google.common.primitives.Longs
import encry.utils.Logging
import encry.view.wallet.WalletHolder.{GetCurrentBalanceRequest, GetCurrentBalanceResponse, UpdateCurrentState}
import akka.util.ByteString
import encry.modifiers.state.box.Box.Amount
import org.encryfoundation.common.utils.TaggedTypes.ADKey

class WalletHolder extends PersistentActor with Logging {

  override def receiveRecover: Receive = changeState()

  override def receiveCommand: Receive = changeState()

  def changeState(currentState: Map[ByteString, Seq[(ADKey, ByteString)]] = Map()): Receive = {
    case UpdateCurrentState(balance, toUpdate, toRemove) =>
      val stateAfterRemove: Map[ByteString, Seq[(ADKey, ByteString)]] = currentState.map ( value =>
        balance -> value._2.filter(k => !toRemove.contains(k._1))
      )
      val stateAfterInsert: Map[ByteString, Seq[(ADKey, ByteString)]] = stateAfterRemove.map ( value =>
        balance -> (value._2 ++ toUpdate)
      )
      persist(stateAfterInsert) { state =>
        logInfo(s"Success save new state. Inner state is: ${state.keySet.head}, ${state.values.head}")
      }
      context.become(changeState(stateAfterInsert))

    case GetCurrentBalanceRequest =>
      val balance: Map[ByteString, Amount] = currentState.keySet.headOption.getOrElse(ByteString.empty)
        .sliding(40, 40)
        .map(byteString => byteString.take(32) -> Longs.fromByteArray(byteString.takeRight(8).toArray))
        .toMap
      sender() ! GetCurrentBalanceResponse(balance)
      context.become(changeState(currentState))

    case restore: Map[ByteString, Seq[(ADKey, ByteString)]] => context.become(changeState(restore))

    case _ => context.become(changeState(currentState))
  }

  override def persistenceId: String = "wallet persistent actor"

  override def journalPluginId: String = "akka.persistence.journal.leveldb"
}

object WalletHolder {

  case class UpdateCurrentState(balance: ByteString,
                                newBoxes: Seq[(ADKey, ByteString)],
                                spentBoxes: Seq[ADKey])

  case class GetCurrentBalanceRequest()

  case class GetCurrentBalanceResponse(balance: Map[ByteString, Amount])

}