package encry.view

import akka.actor.Actor
import encry.EncryApp.{nodeViewSynchronizer, timeProvider}
import encry.consensus.History
import encry.modifiers.{EncryPersistentModifier, NodeViewModifier}
import encry.modifiers.history._
import encry.modifiers.mempool.{Transaction, TransactionSerializer}
import encry.network.DeliveryManager.{ContinueSync, StopSync}
import encry.network.NodeViewSynchronizer.ReceivableMessages._
import encry.settings.EncryAppSettings
import encry.stats.StatsSender._
import encry.utils.CoreTaggedTypes.{ModifierId, ModifierTypeId}
import encry.utils.Logging
import encry.view.EncryNodeViewHolder.ReceivableMessages._
import encry.view.history.EncryHistory
import org.encryfoundation.common.serialization.Serializer

import scala.collection.{Seq, mutable}
import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.concurrent.duration._
import scala.util.Try

class NodeHistoryHolder(var history: EncryHistory, settings: EncryAppSettings) extends Actor with Logging {

  implicit val ec: ExecutionContextExecutor = context.dispatcher

  val modifierSerializers: Map[ModifierTypeId, Serializer[_ <: NodeViewModifier]] = Map(
    Header.modifierTypeId -> HeaderSerializer,
    Payload.modifierTypeId -> PayloadSerializer,
    ADProofs.modifierTypeId -> ADProofSerializer,
    Transaction.ModifierTypeId -> TransactionSerializer
  )

  if (settings.influxDB.isDefined) {
    context.system.scheduler.schedule(5.second, 5.second) {
      context.system.actorSelection("user/statsSender") !
        HeightStatistics(history.bestHeaderHeight, history.bestBlockHeight)
    }
  }

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = reason.printStackTrace()

  override def receive: Receive = {
    case GetFromCurrentHistory(f) =>
      val result = f(history)
      result match {
        case resultFuture: Future[_] => resultFuture.map(sender ! FromCurrentHistory(_))
        case _ => sender() ! FromCurrentHistory(result)
      }
    case HistoryUpdateFromNVH(newHistory, publish) =>
      history = newHistory
      if (publish) context.system.eventStream.publish(ChangedHistory(history))
    case CompareViews(peer, modifierTypeId, modifierIds) if modifierTypeId != Transaction.ModifierTypeId =>
      val ids: Seq[ModifierId] = modifierIds.filterNot(mid => history.contains(mid) || ModifiersCache.contains(key(mid)))
      sender() ! RequestFromLocal(peer, modifierTypeId, ids)
    case _: GetNodeViewChanges => sender() ! ChangedHistory(history)
    case ModifiersFromRemote(modifierTypeId, remoteObjects) =>
      if (ModifiersCache.isEmpty && history.isHeadersChainSynced) nodeViewSynchronizer ! StopSync
      modifierSerializers.get(modifierTypeId).foreach { companion =>
        remoteObjects.flatMap(r => companion.parseBytes(r).toOption).foreach {
          case tx: Transaction@unchecked if tx.modifierTypeId == Transaction.ModifierTypeId => context.parent ! TxForTxModify(tx)
          case pmod: EncryPersistentModifier@unchecked =>
            if (settings.influxDB.isDefined && history.isFullChainSynced) {
              pmod match {
                case h: Header => context.system.actorSelection("user/statsSender") ! TimestampDifference(timeProvider.estimatedTime - h.timestamp)
                case b: Block => context.system.actorSelection("user/statsSender") ! TimestampDifference(timeProvider.estimatedTime - b.header.timestamp)
                case _ =>
              }
            }
            if (history.contains(pmod.id) || ModifiersCache.contains(key(pmod.id)))
              logWarn(s"Received modifier ${pmod.encodedId} that is already in history")
            else {
              ModifiersCache.put(key(pmod.id), pmod, history)
            }
        }
        logInfo(s"Cache before(${ModifiersCache.size})")
        computeApplications()
        if (ModifiersCache.isEmpty || !history.isHeadersChainSynced) nodeViewSynchronizer ! ContinueSync
        logInfo(s"Cache after(${ModifiersCache.size})")
      }
    case pmod: EncryPersistentModifier if !history.contains(pmod.id) => pmodModify(pmod)
  }

  def pmodModify(pmod: EncryPersistentModifier): Unit = if (!history.contains(pmod.id)) {
    logInfo(s"Apply modifier ${pmod.encodedId} of type ${pmod.modifierTypeId} to nodeHistoryHolder")
    if (settings.influxDB.isDefined) context.system
      .actorSelection("user/statsSender") !
      StartApplyingModif(pmod.id, pmod.modifierTypeId, System.currentTimeMillis())
    val historyAppendTry: Try[(EncryHistory, History.ProgressInfo[EncryPersistentModifier])] = history.append(pmod)
    context.parent ! HistoryUpdate(historyAppendTry, pmod)
  }

  def computeApplications(): Unit =
    ModifiersCache.popCandidate(history) match {
      case Some(payload) =>
        pmodModify(payload)
        computeApplications()
      case None => Unit
    }

  def key(id: ModifierId): mutable.WrappedArray.ofByte = new mutable.WrappedArray.ofByte(id)

  override def postStop(): Unit = history.closeStorage()
}

case class FromCurrentHistory[A](value: A)
case class NewModifier(pmod: EncryPersistentModifier)
case class TxForTxModify(tx: Transaction)
case class HistoryUpdate(historyUpdate: Try[(EncryHistory, History.ProgressInfo[EncryPersistentModifier])], pmod: EncryPersistentModifier)
case class HistoryUpdateFromNVH(history: EncryHistory, publish: Boolean)
