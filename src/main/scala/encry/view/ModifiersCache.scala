package encry.view

import java.util.concurrent.ConcurrentHashMap
import encry.EncryApp.settings
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.Header
import encry.utils.CoreTaggedTypes.ModifierId
import encry.utils.Logging
import encry.validation.{MalformedModifierError, RecoverableModifierError}
import encry.view.history.EncryHistory
import org.encryfoundation.common.Algos
import scala.collection.concurrent.TrieMap
import scala.collection.immutable.SortedMap
import scala.collection.mutable
import scala.util.{Failure, Success}

object ModifiersCache extends Logging {

  private type Key = mutable.WrappedArray[Byte]

  private val cache: TrieMap[Key, EncryPersistentModifier] = TrieMap[Key, EncryPersistentModifier]()
  private var cleaning: Boolean = settings.postgres.forall(postgres => !postgres.enableRestore) &&
    settings.levelDb.forall(levelDb => !levelDb.enableRestore)

  def setCleaningToTrue(): Unit = cleaning = true

  def size: Int = cache.size

  def isEmpty: Boolean = size == 0

  def contains(key: Key): Boolean = cache.contains(key) || ConcurrentHashMap.newKeySet[Key]().contains(key)

  def put(key: Key, value: EncryPersistentModifier, history: EncryHistory): Unit = if (!contains(key)) {
    cache.put(key, value)
    if (size > settings.node.modifiersCacheSize && cleaning) cache.find {
      case (_, value) => history.testApplicable(value) match {
        case Success(_) => false
        case Failure(_: RecoverableModifierError) => false
        case _ => true
      }
    }.map(_._1).map(remove)
  }

  def remove(key: Key): Option[EncryPersistentModifier] = {
    logInfo(s"Going to delete ${Algos.encode(key.toArray)}. Cache contains : ${cache.get(key).isDefined}")
    cache.remove(key).map { removed =>
      removed
    }
  }

  def popCandidate(history: EncryHistory): Option[EncryPersistentModifier] = synchronized {
    findCandidateKey(history).flatMap(k => remove(k))
  }

  override def toString: String = cache.keys.map(key => Algos.encode(key.toArray)).mkString(",")

  def findCandidateKey(history: EncryHistory): Option[Key] = {

    def isApplicable(key: Key): Boolean = cache.get(key).exists(modifier => history.testApplicable(modifier) match {
      case Failure(_: RecoverableModifierError) => false
      case Failure(_: MalformedModifierError) =>
        remove(key)
        false
      case Failure(_) => false
      case m => m.isSuccess
    }
    )
    cache.find { case (k, v) =>
      v match {
        case _: Header
          if history.bestHeaderOpt.exists(header => header.id sameElements v.parentId) =>
          true
        case _ =>
          logError(s"Try to apply: ${Algos.encode(k.toArray)} and result is: ${isApplicable(k)}")
          isApplicable(k)
      }
    }.map { case (k, _) => k }
  }
}