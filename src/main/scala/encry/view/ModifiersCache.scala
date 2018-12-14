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
  private var headersQueue: SortedMap[Int, Seq[Key]] = SortedMap.empty[Int, Seq[Key]]
  private var cleaning: Boolean = settings.postgres.forall(postgres => !postgres.enableRestore) &&
    settings.levelDb.forall(levelDb => !levelDb.enableRestore)

  def setCleaningToTrue(): Unit = cleaning = true

  def size: Int = cache.size

  def isEmpty: Boolean = size == 0

  def contains(key: Key): Boolean = cache.contains(key) || ConcurrentHashMap.newKeySet[Key]().contains(key)

  def put(key: Key, value: EncryPersistentModifier, history: EncryHistory): Unit = if (!contains(key)) {
    cache.put(key, value)
    value match {
      case header: Header =>
        headersQueue = headersQueue.updated(
          header.height,
          headersQueue.getOrElse(header.height, Seq.empty) :+ new mutable.WrappedArray.ofByte(header.id)
        )
      case _ =>
    }
    if (size > settings.node.modifiersCacheSize && cleaning) cache.find {
      case (_, value) => history.testApplicable(value) match {
        case Success(_) => false
        case Failure(_: RecoverableModifierError) => false
        case _ => true
      }
    }.map(_._1).map(remove)
  }

  def remove(key: Key): Option[EncryPersistentModifier] = {
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

    val headersIdAfterLastFullBlock: Seq[ModifierId] =
      history.headerIdsAtHeight(history.bestBlockHeight + 1)

    val headersAfterLastFullBlock: Seq[Header] =
      headersIdAfterLastFullBlock.flatMap(id => history.typedModifierById[Header](id))

    val payloadIds: Seq[mutable.WrappedArray[Byte]] = headersAfterLastFullBlock
      .map(header => new mutable.WrappedArray.ofByte(header.parentId))

    val payloadIdsAndPayloadsInCache: Map[mutable.WrappedArray[Byte], EncryPersistentModifier] =
      payloadIds.flatMap(payloadId => cache.get(payloadId).map(payload => payloadId -> payload)).toMap

    val applicablePayload: Option[mutable.WrappedArray[Byte]] =
      payloadIdsAndPayloadsInCache.find(payloadIdAndPayloadInCache => isApplicable(payloadIdAndPayloadInCache._1)).map(_._1)

    applicablePayload.orElse {
      val possibleHeaders: Seq[Key] =
        headersQueue.get(history.bestHeaderOpt.map(_.height).getOrElse(0) + 1).map(headersKey =>
          headersKey.filter(isApplicable)
        ).getOrElse(Seq.empty)
      logError(s"possibleHeaders: ${possibleHeaders.map(key => Algos.encode(key.toArray)).mkString(",")}")
      headersQueue.get(history.bestHeaderHeight + 1).flatMap(_.headOption).orElse {
        if (possibleHeaders.nonEmpty) Some(possibleHeaders.head)
        else
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
  }
}