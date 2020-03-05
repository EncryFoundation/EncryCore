package encry.view.mempool

import java.net.InetSocketAddress
import akka.actor.{ Actor, Props }
import com.google.common.base.Charsets
import com.google.common.hash.{ BloomFilter, Funnels }
import com.typesafe.scalalogging.StrictLogging
import encry.network.Messages.MessageToNetwork.RequestFromLocal
import encry.network.NetworkController.ReceivableMessages.DataFromPeer
import encry.settings.EncryAppSettings
import encry.view.mempool.IntermediaryMempool.IsChainSynced
import encry.view.mempool.MemoryPool.TransactionProcessing
import encry.view.mempool.MemoryPoolProcessor.{ CleanupBloomFilter, RequestedModifiersForRemote }
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.network.BasicMessagesRepo.{ InvNetworkMessage, RequestModifiersNetworkMessage }
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
import scala.collection.IndexedSeq

class MemoryPoolProcessor(settings: EncryAppSettings) extends Actor with StrictLogging {

  import context.dispatcher

  var bloomFilterForTransactionsIds: BloomFilter[String] = initBloomFilter

  var canProcessTransactions: Boolean = false

  var chainSynced: Boolean = false

  override def preStart(): Unit =
    context.system.scheduler.schedule(settings.mempool.bloomFilterCleanupInterval,
                                      settings.mempool.bloomFilterCleanupInterval,
                                      self,
                                      CleanupBloomFilter)

  override def receive: Receive = {

    case TransactionProcessing(info) => canProcessTransactions = info

    case IsChainSynced(info) => chainSynced = info

    case CleanupBloomFilter =>
      bloomFilterForTransactionsIds = initBloomFilter

    case DataFromPeer(message, remote) =>
      message match {

        case RequestModifiersNetworkMessage((_, requestedIds)) =>
          val modifiersIds: Seq[Transaction] = requestedIds
            .map(Algos.encode)
            .collect { case id if memoryPool.contains(id) => memoryPool.get(id) }
            .flatten
          context.parent ! RequestedModifiersForRemote(remote, modifiersIds)
          logger.debug(
            s"MemoryPool got request modifiers message. Number of requested ids is ${requestedIds.size}." +
              s" Number of sent transactions is ${modifiersIds.size}. Request was from $remote."
          )

        case InvNetworkMessage((_, txs)) =>
          val notYetRequestedTransactions: IndexedSeq[ModifierId] = notRequestedYet(txs.toIndexedSeq)
          if (notYetRequestedTransactions.nonEmpty) {
            sender ! RequestFromLocal(Some(remote), Transaction.modifierTypeId, notYetRequestedTransactions.toList)
            logger.debug(
              s"MemoryPool got inv message with ${txs.size} ids." +
                s" Not yet requested ids size is ${notYetRequestedTransactions.size}."
            )
          } else
            logger.debug(
              s"MemoryPool got inv message with ${txs.size} ids." +
                s" There are no not yet requested ids."
            )

        case InvNetworkMessage(invData) =>
          logger.debug(
            s"Get inv with tx: ${invData._2.map(Algos.encode).mkString(",")}, but " +
              s"chainSynced is $chainSynced and canProcessTransactions is $canProcessTransactions."
          )

        case _ => logger.debug(s"MemoryPoolProcessor got invalid type of DataFromPeer message!")
      }

  }
  def initBloomFilter: BloomFilter[String] = BloomFilter.create(
    Funnels.stringFunnel(Charsets.UTF_8),
    settings.mempool.bloomFilterCapacity,
    settings.mempool.bloomFilterFailureProbability
  )

  def notRequestedYet(ids: IndexedSeq[ModifierId]): IndexedSeq[ModifierId] = ids.collect {
    case id: ModifierId if !bloomFilterForTransactionsIds.mightContain(Algos.encode(id)) =>
      bloomFilterForTransactionsIds.put(Algos.encode(id))
      id
  }

}

object MemoryPoolProcessor {

  def props(settings: EncryAppSettings) = Props(new MemoryPoolProcessor(settings))

  case object CleanupBloomFilter

  final case class RequestedModifiersForRemote(peer: InetSocketAddress, txs: Seq[Transaction])

}
