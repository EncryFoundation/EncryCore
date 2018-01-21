package encry.network

import akka.actor.{ActorContext, ActorRef}
import encry.view.history.EncryHistoryReader
import scorex.core.network.{ConnectedPeer, DeliveryTracker}
import scorex.core.utils.NetworkTimeProvider
import scorex.core.{ModifierId, ModifierTypeId}

import scala.collection.mutable
import scala.concurrent.duration._

class EncryDeliveryTracker(context: ActorContext,
                           deliveryTimeout: FiniteDuration,
                           maxDeliveryChecks: Int,
                           nvsRef: ActorRef,
                           timeProvider: NetworkTimeProvider)
  extends DeliveryTracker(context, deliveryTimeout, maxDeliveryChecks, nvsRef) {

  val toDownload: mutable.Map[ModifierIdAsKey, ToDownloadStatus] = mutable.Map[ModifierIdAsKey, ToDownloadStatus]()
  //TODO move to config?
  private val toDownloadRetryInterval = 30.seconds
  private val toDownloadLifetime = 1.hour

  def downloadRequested(modifierTypeId: ModifierTypeId, modifierId: ModifierId): Unit = {
    val time = timeProvider.time()
    val prevValue = toDownload.get(key(modifierId))
    val newValue = prevValue.map(p => p.copy(lastTry = time)).getOrElse(ToDownloadStatus(modifierTypeId, time, time))
    toDownload.put(key(modifierId), newValue)
  }

  override def receive(mtId: ModifierTypeId, mId: ModifierId, cp: ConnectedPeer): Unit = {
    if (isExpecting(mtId, mId, cp) || toDownload.contains(key(mId))) {
      toDownload.remove(key(mId))
      val eo = expecting.find(e => (mtId == e._1) && (mId sameElements e._2) && cp == e._3)
      for (e <- eo) expecting -= e
      delivered(key(mId)) = cp
    } else {
      deliveredSpam(key(mId)) = cp
    }
  }

  def removeOutdatedToDownload(historyReaderOpt: Option[EncryHistoryReader]): Unit = {
    val currentTime = timeProvider.time()
    toDownload.filter(td => (td._2.firstViewed < currentTime - toDownloadLifetime.toMillis)
      || historyReaderOpt.exists(hr => hr.contains(ModifierId @@ td._1.array)))
      .foreach(i => toDownload.remove(i._1))
  }

  def downloadRetry(): Seq[(ModifierId, ToDownloadStatus)] = {
    val currentTime = timeProvider.time()
    toDownload.filter(_._2.lastTry < currentTime - toDownloadRetryInterval.toMillis)
      .map(i => (ModifierId @@ i._1.array, i._2)).toSeq
  }
}
