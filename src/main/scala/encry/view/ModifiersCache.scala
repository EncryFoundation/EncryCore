package encry.view

import java.lang
import java.util.concurrent.ConcurrentHashMap
import encry.EncryApp.settings
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.Header
import encry.utils.Logging
import encry.validation.{MalformedModifierError, RecoverableModifierError}
import encry.view.history.EncryHistory
import org.encryfoundation.common.Algos
import scala.collection.concurrent.TrieMap
import scala.collection.immutable.SortedMap
import scala.collection.mutable
import scala.util.{Failure, Success}

case class ModifiersCache(maxSize: Int) extends Logging {

  type K = mutable.WrappedArray[Byte]
  type V = EncryPersistentModifier

  val cache: TrieMap[K, V] = TrieMap[K, V]()

  private var headersQueue: SortedMap[Int, Seq[K]] = SortedMap.empty[Int, Seq[K]]

  private var cleaning: Boolean = settings.postgres.forall(postgres => !postgres.enableRestore) &&
    settings.levelDb.forall(levelDb => !levelDb.enableRestore)

  protected val rememberedKeys: ConcurrentHashMap.KeySetView[K, lang.Boolean] = ConcurrentHashMap.newKeySet[K]()

  def enableCleaning: Unit = cleaning = true

  def size: Int = cache.size

  def isEmpty: Boolean = size == 0

  /**
    * Keys to simulate objects residing a cache. So if key is stored here,
    * the membership check (contains()) shows that the key is in the cache,
    * but the value corresponding to the key is not stored. The motivation
    * to have this structure is to avoid repeatedly downloading modifiers
    * which are unquestionably invalid.
    */

  protected def onPut(key: K): Unit = {}

  protected def onRemove(key: K): Unit = {}

  /**
    * A cache element replacement strategy method, which defines a key to remove from cache when it is overfull
    */
  protected def keyToRemove(history: EncryHistory): Option[K] = cache.find {
    case (_, value) => history.testApplicable(value) match {
      case Success(_) => false
      case Failure(_: RecoverableModifierError) => false
      case _ => true
    }
  }.map(_._1)

  def contains(key: K): Boolean = cache.contains(key) || rememberedKeys.contains(key)

  def put(key: K, value: V, history: EncryHistory): Unit = if (!contains(key)) {
      cache.put(key, value)
      value match {
        case header: Header =>
          headersQueue = headersQueue.updated(
            header.height,
            headersQueue.getOrElse(header.height, Seq.empty) :+ new mutable.WrappedArray.ofByte(header.id)
          )
        case _ =>
      }
      if (size > maxSize && cleaning) keyToRemove(history).map(remove)
    }


  def remove(key: K): Option[V] = {
    cache.get(key).foreach {
      case header: Header =>
        headersQueue = headersQueue.updated(
          header.height,
          headersQueue.getOrElse(header.height, Seq.empty).diff(new mutable.WrappedArray.ofByte(header.id))
        )
      case _ =>
    }
    cache.remove(key).map { removed =>
      removed
    }
  }

  def popCandidate(history: EncryHistory): Option[V] = synchronized {
    findCandidateKey(history).flatMap(k => remove(k))
  }

  override def toString: String = cache.keys.map(key => Algos.encode(key.toArray)).mkString(",")

  def findCandidateKey(history: EncryHistory): Option[K] = {

    def tryToApply(k: K): Boolean = cache.get(k).exists(modifier => history.testApplicable(modifier) match {
      case Failure(_: RecoverableModifierError) => false
      case Failure(_: MalformedModifierError) =>
        remove(k)
        false
      case Failure(_) => false
      case m => m.isSuccess
    }
    )

    history.headerIdsAtHeight(history.bestBlockHeight + 1)
      .flatMap(id => history.typedModifierById[Header](id))
      .flatMap(_.partsIds.map(id => mutable.WrappedArray.make[Byte](id)))
      .flatMap(id => cache.get(id).map(v => id -> v))
      .find(p => tryToApply(p._1)).map(_._1)
      .orElse {
        // do exhaustive search between modifiers, that are possibly may be applied (exclude headers far from best header)
        val possibleHeaders: Seq[K] =
          headersQueue.get(history.bestHeaderOpt.map(_.height).getOrElse(0) + 1).map(headersKey =>
          headersKey.filter(tryToApply)
        ).getOrElse(Seq.empty)
        headersQueue.get(history.bestHeaderHeight + 1).flatMap(_.headOption).orElse {
          if (possibleHeaders.nonEmpty) Some(possibleHeaders.head)
          else
            cache.find { case (k, v) =>
              v match {
                case _: Header if history.bestHeaderOpt.exists(header => header.id sameElements v.parentId) =>
                  true
                case _ =>
                  tryToApply(k)
              }
            }.map { case (k, _) => k }
        }
      }
  }
}

