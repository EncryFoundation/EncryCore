package encry.view

import java.lang
import java.util.concurrent.ConcurrentHashMap

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.Header
import encry.EncryApp.settings
import encry.modifiers.history.{Header, Payload}
import encry.utils.CoreTaggedTypes.ModifierId
import encry.validation.{MalformedModifierError, RecoverableModifierError}
import encry.view.history.{EncryHistory, EncryHistoryReader}
import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.util.{Failure, Success}
import encry.utils.Logging
import org.encryfoundation.common.Algos

trait ModifiersCache[PMOD <: EncryPersistentModifier, H <: EncryHistoryReader] {

  type K = mutable.WrappedArray[Byte]
  type V = PMOD

  val cache: TrieMap[K, V] = TrieMap[K, V]()

  private var cleaning: Boolean = settings.postgres.forall(postgres => !postgres.enableRestore) &&
    settings.levelDb.forall(levelDb => !levelDb.enableRestore)

  def enableCleaning: Unit = cleaning = true

  def size: Int = cache.size

  def isEmpty: Boolean = size == 0

  def maxSize: Int

  /**
    * Keys to simulate objects residing a cache. So if key is stored here,
    * the membership check (contains()) shows that the key is in the cache,
    * but the value corresponding to the key is not stored. The motivation
    * to have this structure is to avoid repeatedly downloading modifiers
    * which are unquestionably invalid.
    */
  protected val rememberedKeys: ConcurrentHashMap.KeySetView[K, lang.Boolean] = ConcurrentHashMap.newKeySet[K]()

  /**
    * Defines a best (and application-specific) candidate to be applied.
    *
    * @param history - an interface to history which could be needed to define a candidate
    * @return - candidate if it is found
    */
  def findCandidateKey(history: H): Option[K]

  protected def onPut(key: K): Unit = {}

  protected def onRemove(key: K): Unit = {}

  /**
    * A cache element replacement strategy method, which defines a key to remove from cache when it is overfull
    */
  protected def keyToRemove(): K


  def contains(key: K): Boolean = cache.contains(key) || rememberedKeys.contains(key)

  def put(key: K, value: V): Unit =
    if (!contains(key)) {
      onPut(key)
      cache.put(key, value)
      if (size > maxSize && cleaning) remove(keyToRemove())
    }

  def remove(key: K): Option[V] = {
    cache.remove(key).map { removed =>
      onRemove(key)
      removed
    }
  }

  def popCandidate(history: H): Option[V] = synchronized {
    findCandidateKey(history).flatMap(k => remove(k))
  }

  override def toString: String = cache.keys.map(key => Algos.encode(key.toArray)).mkString(",")
}

trait LRUCache[PMOD <: EncryPersistentModifier, HR <: EncryHistoryReader] extends ModifiersCache[PMOD, HR] {

  private val evictionQueue = mutable.Queue[K]()

  // The eviction queue can contain elements already removed, as we're not removing a key from it when
  // the key is got removed from the cache. When size of eviction queue exceeds maximum size of the cache by
  // the value below(so the queue contains at least "cleaningThreshold" keys aleady removed from the cache),
  // complete scan and cleaning of removed keys happen.
  private val cleaningThreshold = 50

  @tailrec
  private def evictionCandidate(): K = {
    val k = evictionQueue.dequeue()
    if (cache.contains(k)) k else evictionCandidate()
  }

  override protected def onPut(key: K): Unit = {
    evictionQueue.enqueue(key)
    if (evictionQueue.size > maxSize + cleaningThreshold) {
      evictionQueue.dequeueAll(k => !cache.contains(k))
    }
  }

  override protected def onRemove(key: K): Unit = {}

  def keyToRemove(): K = evictionCandidate()
}

class DefaultModifiersCache[PMOD <: EncryPersistentModifier, HR <: EncryHistoryReader]
(override val maxSize: Int) extends LRUCache[PMOD, HR] with Logging {

  /**
    * Default implementation is just about to scan. Not efficient at all and should be probably rewritten in a
    * concrete application.
    *
    * @param history - an interface to history which could be needed to define a candidate
    * @return - candidate if it is found
    */
  @SuppressWarnings(Array("org.wartremover.warts.IsInstanceOf"))
  override def findCandidateKey(history: HR): Option[K] = {
    cache.find { case (k, v) =>
      history.testApplicable(v) match {
        case Failure(e: RecoverableModifierError) =>
          logInfo(s"Error: $e")
          false
        case Failure(e: MalformedModifierError) =>
          logWarn(s"Modifier ${v.encodedId} is permanently invalid and will be removed from cache caused $e")
          remove(k)
          false
        case Failure(e) =>
          logWarn(s"Modifier ${v.encodedId} is permanently invalid and will be removed from cache caused $e")
          remove(k)
          false
        case Success(_) =>
          true
      }
    }.map { case (k, _) => k }
  }
}

case class EncryModifiersCache(override val maxSize: Int)
  extends DefaultModifiersCache[EncryPersistentModifier, EncryHistory](maxSize) {

  override def findCandidateKey(history: EncryHistory): Option[K] = {
    def tryToApply(k: K, v: EncryPersistentModifier): Boolean = {
      history.testApplicable(v) match {
        case Failure(e: RecoverableModifierError) =>
          logInfo(s"Error: $e")
          false
        case Failure(e: MalformedModifierError) =>
          logWarn(s"Modifier ${v.encodedId} is permanently invalid and will be removed from cache caused $e")
          remove(k)
          false
        case Failure(exception) =>
          logWarn(s"Exception during apply modifier ${Algos.encode(v.id)} $exception")
          false
        case m => m.isSuccess
      }
    }

    val headersHeight = history.bestHeaderHeight

    history
      .headerIdsAtHeight(history.bestBlockHeight + 1)
      .flatMap(id => history.typedModifierById[Header](id))
      .flatMap(_.partsIds.map(id => mutable.WrappedArray.make[Byte](id)))
      .flatMap(id => cache.get(id).map(v => id -> v))
      .find(p => tryToApply(p._1, p._2)).map(_._1)
      .orElse {
        // do exhaustive search between modifiers, that are possibly may be applied (exclude headers far from best header)
        cache.find { case (k, v) =>
          v match {
            case h: Header if h.height > headersHeight + 1 => false
            case _ => tryToApply(k, v)
          }
        }.map { case (k, _) => k }
      }
  }
}

