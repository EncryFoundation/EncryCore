package encry.view

import akka.actor.{Actor, ActorSystem, Props}
import encry.modifiers.mempool.Transaction
import encry.settings.EncryAppSettings
import encry.utils.NetworkTimeProvider
import encry.view.MemoryPool.TransactionsFromNetwork
import encry.view.mempool.Mempool

import scala.concurrent.ExecutionContextExecutor

class MemoryPool(settings: EncryAppSettings, ntp: NetworkTimeProvider) extends Actor {

  implicit val ec: ExecutionContextExecutor = context.dispatcher
  implicit val system: ActorSystem = context.system

  val memPool: Mempool = Mempool.empty(settings, ntp, system)

  var memoryPool: IndexedSeq[Transaction] = IndexedSeq.empty[Transaction]

  override def receive: Receive = {
    case TransactionsFromNetwork(transactions) =>
  }


}

object MemoryPool {

  case class TransactionsFromNetwork(transactions: IndexedSeq[Transaction])

  def props: Props = Props(new MemoryPool)
}