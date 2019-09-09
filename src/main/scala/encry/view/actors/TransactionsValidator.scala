package encry.view.actors

import akka.actor.{Actor, Props}
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.EncrySupplyController
import encry.view.actors.TransactionsValidator.{StartValidation, TransactionValidatedFailure, TransactionValidatedSuccessfully}
import encry.view.state.UtxoState
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.utils.TaggedTypes.Height
import org.encryfoundation.common.validation.ValidationResult

class TransactionsValidator(state: UtxoState,
                            totalFees: Amount,
                            transaction: Transaction,
                            height: Height) extends Actor with StrictLogging {

  override def preStart(): Unit = self ! StartValidation

  override def receive: Receive = {
    case StartValidation =>
      state.validate(transaction, totalFees) match {
        case Left(value) => context.parent ! TransactionValidatedFailure(transaction, value)
        case Right(value) => context.parent ! TransactionValidatedSuccessfully(value)
      }
      context.stop(self)
  }
}

object TransactionsValidator {
  def props(state: UtxoState,
            totalFees: Amount,
            transaction: Transaction,
            height: Height): Props = Props(new TransactionsValidator(state, totalFees, transaction, height))

  case object StartValidation
  final case class TransactionValidatedSuccessfully(tx: Transaction) extends AnyVal
  final case class TransactionValidatedFailure(tx: Transaction, exceptions: ValidationResult)
}