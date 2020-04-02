package encry.mpg

import akka.actor.{ Actor, Props }
import com.google.common.base.Charsets
import com.google.common.hash.{ BloomFilter, Funnels }
import com.typesafe.scalalogging.StrictLogging
import encry.network.DeliveryManager.FullBlockChainIsSynced
import encry.network.Messages.MessageToNetwork.{ RequestFromLocal, ResponseFromLocal }
import encry.network.NetworkController.ReceivableMessages.DataFromPeer
import encry.settings.EncryAppSettings
import encry.utils.NetworkTimeProvider
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.network.BasicMessagesRepo.{ InvNetworkMessage, RequestModifiersNetworkMessage }
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
import encry.mpg.MemoryPool._
import encry.mpg.MemoryPoolProcessor.CleanupBloomFilter

import scala.collection.IndexedSeq

class MemoryPoolProcessor(settings: EncryAppSettings, ntp: NetworkTimeProvider) extends Actor with StrictLogging {

  import context.dispatcher

  var bloomFilterForTransactionsIds: BloomFilter[String] = initBloomFilter

  var canProcessTransactions: Boolean = false

  var chainSynced: Boolean = false

  var memoryPoolReader: MemoryPoolReader = MemoryPoolReader.empty

  override def preStart(): Unit =
    context.system.scheduler.schedule(
      settings.mempool.bloomFilterCleanupInterval,
      settings.mempool.bloomFilterCleanupInterval,
      self,
      CleanupBloomFilter
    )

  override def receive: Receive = {
    case UpdateMempoolReader(reader) => memoryPoolReader = reader
    case TransactionProcessing(info) => canProcessTransactions = info
    case FullBlockChainIsSynced      => chainSynced = true
    case CleanupBloomFilter          => bloomFilterForTransactionsIds = initBloomFilter
    case DataFromPeer(message, remote) =>
      message match {
        case RequestModifiersNetworkMessage((_, requestedIds)) =>
          val modifiersIds: Seq[Transaction] = requestedIds
            .map(Algos.encode)
            .flatMap(memoryPoolReader.get)
          logger.debug(
            s"MemoryPool got request modifiers message. Number of requested ids is ${requestedIds.size}." +
              s" Number of sent transactions is ${modifiersIds.size}. Request was from $remote."
          )
          context.parent ! ResponseFromLocal(
            remote,
            Transaction.modifierTypeId,
            modifiersIds.map(tx => tx.id -> tx.bytes).toMap
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

  def props(settings: EncryAppSettings, ntp: NetworkTimeProvider): Props = Props(new MemoryPoolProcessor(settings, ntp))

  case object CleanupBloomFilter
}
