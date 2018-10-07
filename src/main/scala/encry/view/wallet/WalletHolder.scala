package encry.view.wallet

import akka.persistence.PersistentActor
import com.google.common.primitives.Longs
import encry.utils.Logging
import encry.view.wallet.WalletHolder.{CheckIfContainsBox, GetCurrentBalanceRequest, GetCurrentBalanceResponse, UpdateCurrentState}
import akka.util.ByteString
import encry.modifiers.state.box.Box.Amount
import encry.modifiers.state.StateModifierDeserializer
import encry.modifiers.state.box.EncryBaseBox
import org.encryfoundation.common.transaction.Input
import org.encryfoundation.common.utils.TaggedTypes.ADKey
import org.encryfoundation.common.Algos
import scala.util.{Failure, Success}

class WalletHolder extends PersistentActor with Logging {

  override def receiveRecover: Receive = changeState()

  override def receiveCommand: Receive = changeState()

  def changeState(currentState: Map[ByteString, Seq[(ADKey, ByteString)]] = Map()): Receive = {
    case UpdateCurrentState(balance, toUpdate, toRemove) =>
      val stateAfterRemove: Map[ByteString, Seq[(ADKey, ByteString)]] = currentState.map(value =>
        balance -> value._2.filter(k => !toRemove.contains(k._1))
      )
      val stateAfterInsert: Map[ByteString, Seq[(ADKey, ByteString)]] = stateAfterRemove.map(value =>
        balance -> (value._2 ++ toUpdate)
      )
      persist(stateAfterInsert) { state =>
        logInfo(s"Success saved new state. Inner state is: " +
          s"Balance: ${Algos.encode(state.keySet.head.toArray)}, " +
          s"number of boxes: ${state.values.size}, " +
          s"boxes state: ${state.values.map(_.map(x => x._1 -> x._2))}.")
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

    case CheckIfContainsBox(inputs) =>
      val boxesWhichContains: Seq[EncryBaseBox] = currentState.values
        .flatMap(_.filter(x => inputs.contains(x._1)))
        .map(x => StateModifierDeserializer.parseBytes(x._2.toArray, x._1.head))
        .foldLeft(Seq[EncryBaseBox]()) { case (futureS, currentS) => currentS match {
          case Success(value) => futureS :+ value
          case Failure(ex) =>
            logError(s"box is invalid because: ${ex.getStackTrace}")
            futureS
        }
        }

      sender() ! boxesWhichContains

    case _ =>
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

  case class CheckIfContainsBox(inputs: IndexedSeq[Input])

}