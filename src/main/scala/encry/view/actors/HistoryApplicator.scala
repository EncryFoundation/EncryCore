package encry.view.actors

import akka.actor.{Actor, Props}
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.HistoryConsensus.ProgressInfo
import encry.network.NodeViewSynchronizer.ReceivableMessages.SyntacticallyFailedModification
import encry.settings.EncryAppSettings
import encry.view.ModifiersCache
import encry.view.ModifiersCache.{Key, cache, logger}
import encry.view.NodeViewErrors.ModifierApplyError.HistoryApplyError
import encry.view.actors.HistoryApplicator._
import encry.view.actors.StateApplicator.RequestNextModifier
import encry.view.history.History
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{Header, Payload}
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId

import scala.collection.immutable.{HashMap, Queue}
import scala.collection.{IndexedSeq, immutable, mutable}

class HistoryApplicator(history: History, setting: EncryAppSettings) extends Actor with StrictLogging {

  final case class QueueElement(modifier: PersistentModifier, progressInfo: ProgressInfo)
  var modifiersQueue: Queue[QueueElement] = Queue.empty[QueueElement]
  var currentQueueLength: Int = 0

  var payloadsCache: HashMap[String, Payload] = HashMap.empty[String, Payload]
  var headersCache: HashMap[String, Header] = HashMap.empty[String, Header]

  var modifiersCache: HashMap[String, PersistentModifier] = HashMap.empty[String, PersistentModifier]



  override def receive: Receive = modifiersFromNetwork
    .orElse(processingModifier)

  def modifiersFromNetwork: Receive = {
    case ModifierFromNetwork(mod) if !history.isModifierDefined(mod.id) && modifiersCache.contains(mod.encodedId) =>
      logger.info(s"Got new modifier ${mod.encodedId} from network. Put it into modifiers cache")
      // simplest cache realization
      //ModifiersCache.put(key(mod.id), mod, history)
      mod match {
        case h: Header if !headersCache.contains(h.encodedId) =>
          logger.info(s"New header ${h.encodedId} added to headers collection")
          headersCache = headersCache.updated(h.encodedId, h)
        case p: Payload if !payloadsCache.contains(p.encodedId) =>
          logger.info(s"New payload ${p.encodedId} added to payloads collection")
          payloadsCache = payloadsCache.updated(p.encodedId, p)
        case _ =>
      }
      if (currentQueueLength < setting.levelDB.maxVersions) {
        logger.info(s"It's possible to append new modifier to history")
        //val modifiersForAppending: List[PersistentModifier] = ModifiersCache.popCandidate(history)
        val modifierForApplying: Option[PersistentModifier] = modifiersCache.find { case (_, v) => v match {
          case v@(_: Header) if history.getBestHeaderId.exists(headerId => headerId sameElements v.parentId) => true
          case v => history.testApplicable(v).isRight
        }}.map(_._2)
        //lotts side effects
        modifierForApplying.foreach { modL =>
          self ! ModifierForHistoryApplicator(modL)
          currentQueueLength += 1
          //for test only
          modifiersCache = modifiersCache - modL.encodedId
        }
      }
    case ModifierFromNetwork(mod) =>
      logger.info(s"Modifier ${mod.encodedId} already contains in history or in modifiers cache. Reject it")
  }

  def processingModifier: Receive = {
    case ModifierForHistoryApplicator(mod) =>
      logger.info(s"Start applying modifier ${mod.encodedId} to history")
      history.append(mod) match {
        case Left(ex) =>
          logger.info(s"Modifier ${mod.encodedId} unsuccessfully applied to history wit exception ${ex.getMessage}")
          context.system.eventStream.publish(SyntacticallyFailedModification(mod, List(HistoryApplyError(ex.getMessage))))
        case Right(progressInfo) =>
          logger.info(s"Modifier ${mod.encodedId} successfully applied to history")
          modifiersQueue = modifiersQueue.enqueue(QueueElement(mod, progressInfo))
          logger.info(s"New element put into queue. Current queue size is ${modifiersQueue.length}")
      }

    case RequestNextModifier =>
      val nextModifier: Option[(QueueElement, Queue[QueueElement])] = modifiersQueue.dequeueOption
      nextModifier.foreach { case (elem, newQueue) =>
        sender() ! ModifierForStateApplicator(elem.modifier, elem.progressInfo)
        currentQueueLength -= 1
        modifiersQueue = newQueue
      }
  }

  def key(id: ModifierId): mutable.WrappedArray.ofByte = new mutable.WrappedArray.ofByte(id)
}

object HistoryApplicator {

  final case class ModifierFromNetwork(mod: PersistentModifier) extends AnyVal
  final case class ModifierForHistoryApplicator(modifier: PersistentModifier) extends AnyVal
  final case class ModifierForStateApplicator(progressInfo: ProgressInfo,
                                              suffixApplied: Seq[PersistentModifier])

  def props(history: History, setting: EncryAppSettings): Props = Props(new HistoryApplicator(history, setting))
}