package encry.view

import java.lang
import java.util.concurrent.ConcurrentHashMap
import encry.EncryApp.settings
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.Header
import encry.utils.Logging
import encry.validation.{MalformedModifierError, RecoverableModifierError}
import encry.view.history.{EncryHistory, EncryHistoryReader}
import org.encryfoundation.common.Algos
import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.util.Failure

trait ModifiersCache[PMOD <: EncryPersistentModifier, H <: EncryHistoryReader] extends Logging {

  type K = mutable.WrappedArray[Byte]
  type V = PMOD

  val cache: TrieMap[K, V] = TrieMap[K, V]()

  val headersQueue: mutable.SortedMap[Int, K] = mutable.SortedMap.empty[Int, K]

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
  protected def keyToRemove(history: EncryHistory): K =
    cache.find(elem => history.testApplicable(elem._2).isFailure).map(_._1).getOrElse(keyToRemove(history))

  def contains(key: K): Boolean = cache.contains(key) || rememberedKeys.contains(key)

  def put(key: K, value: V, history: EncryHistory): Unit = {
    logInfo(s"Trying to put: ${Algos.encode(key.toArray)} to cahce. ${!contains(key)}")
    if (!contains(key)) {
      logInfo(s"Add elem ${Algos.encode(key.toArray)} to cache")
      cache.put(key, value)
      value match {
        case header: Header => headersQueue += (header.height -> key)
        case _ =>
      }
      logInfo(s"Size: $size ? $maxSize and $cleaning")
      if (size > maxSize && cleaning) remove(keyToRemove(history))
    }
  }

  def remove(key: K): Option[V] = {
    cache.remove(key).map { removed =>
      removed
    }
  }

  def popCandidate(history: H): Option[V] = synchronized {
    findCandidateKey(history).flatMap(k => remove(k))
  }

  override def toString: String = cache.keys.map(key => Algos.encode(key.toArray)).mkString(",")
}

case class EncryModifiersCache(override val maxSize: Int)
  extends ModifiersCache[EncryPersistentModifier, EncryHistory] {

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
            case _ => tryToApply(k, v)
          }
        }.map { case (k, _) => k }
      }
  }
}

