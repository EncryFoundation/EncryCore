package encry.view

import java.util.concurrent.ConcurrentHashMap
import com.typesafe.scalalogging.StrictLogging
import encry.EncryApp.settings
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.{Block, Header, Payload}
import encry.utils.CoreTaggedTypes.ModifierId
import encry.modifiers.history.Header
import encry.validation.{MalformedModifierError, RecoverableModifierError}
import encry.view.history.EncryHistory
import org.encryfoundation.common.Algos
import scala.collection.immutable.SortedMap
import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.util.{Failure, Success}

object ModifiersCache extends StrictLogging {

  private type Key = mutable.WrappedArray[Byte]

  private val cache: TrieMap[Key, EncryPersistentModifier] = TrieMap[Key, EncryPersistentModifier]()
  private var headersCollection: SortedMap[Int, List[ModifierId]] = SortedMap[Int, List[ModifierId]]()

  def size: Int = cache.size

  def isEmpty: Boolean = size == 0

  def contains(key: Key): Boolean = cache.contains(key) || ConcurrentHashMap.newKeySet[Key]().contains(key)

  def put(key: Key, value: EncryPersistentModifier, history: EncryHistory): Unit = if (!contains(key)) {
    cache.put(key, value)
    value match {
      case header: Header =>
        val currentHeight: Block.Height = header.height
        val possibleHeadersAtCurrentHeight: List[ModifierId] = headersCollection.getOrElse(currentHeight, List())
        val updatedHeadersAtCurrentHeight: List[ModifierId] = header.id :: possibleHeadersAtCurrentHeight
        headersCollection = headersCollection.updated(currentHeight, updatedHeadersAtCurrentHeight)
      case _ =>
    }
    if (size > settings.node.modifiersCacheSize) cache.find {
      case (_, value) => history.testApplicable(value) match {
        case Success(_) => false
        case Failure(_: RecoverableModifierError) => false
        case _ => true
      }
    }.map(mod => remove(mod._1))
  }

  def remove(key: Key): Option[EncryPersistentModifier] = {
    logger.info(s"Going to delete ${Algos.encode(key.toArray)}. Cache contains: ${cache.get(key).isDefined}")
    cache.remove(key).map(removed => removed)
  }

  def popCandidate(history: EncryHistory): List[EncryPersistentModifier] = synchronized {
    findCandidateKey(history).flatMap(k => remove(k))
  }

  override def toString: String = cache.keys.map(key => Algos.encode(key.toArray)).mkString(",")

  def findCandidateKey(history: EncryHistory): List[Key] = {
    val historyBestHeaderHeight: Int = history.bestHeaderHeight
    val historyBestBlockHeight: Int = history.bestBlockHeight

    def isApplicable(key: Key): Boolean = cache.get(key).exists(modifier => history.testApplicable(modifier) match {
      case Failure(_: RecoverableModifierError) => false
      case Failure(_: MalformedModifierError) =>
        remove(key)
        false
      case Failure(_) => false
      case m => m.isSuccess
    })

    def exhaustiveSearch: List[Key] = List(cache.find { case (k, v) =>
      v match {
        case _: Header if history.bestHeaderOpt.exists(header => header.id sameElements v.parentId) => true
        case _ =>
          val isApplicableMod: Boolean = isApplicable(k)
          //logger.error(s"Try to apply: ${Algos.encode(k.toArray)} and result is: $isApplicableMod")
          isApplicableMod
      }
    }).collect { case Some(v) => v._1 }

    val headerIds: Option[List[ModifierId]] = headersCollection.get(historyBestHeaderHeight + 1)
    val bestHeadersIds: List[Key] = headerIds match {
      case Some(value) =>
        headersCollection = headersCollection - (historyBestHeaderHeight + 1)
        logger.info(s"HeadersCollection size is: ${headersCollection.size}")
        value.map(m => cache.get(m)).collect {
          case Some(v: Header)
            if (v.parentId sameElements history.bestHeaderOpt.map(_.id).getOrElse(Array.emptyByteArray))
              && isApplicable(v.id) =>
            logger.info(s"Find new bestHeader in cache: ${Algos.encode(v.id)}")
            new mutable.WrappedArray.ofByte(v.id).asInstanceOf[Key]
        }
      case None =>
        logger.info(s"No best header in cache")
        List[Key]()
    }
    if (bestHeadersIds.nonEmpty) bestHeadersIds
    else if (historyBestHeaderHeight - historyBestBlockHeight > 0) {
      val payloadId: Option[Key] = history.headerIdsAtHeight(historyBestBlockHeight + 1).headOption match {
        case Some(id) =>
          val header: Option[Header] = history.modifierById(id).collect { case header: Header => header }
          val modFromCache: Option[EncryPersistentModifier] = cache.get {
            val payloadId: Array[Byte] = header.map(_.payloadId).getOrElse(Array.emptyByteArray)
            new mutable.WrappedArray.ofByte(payloadId).asInstanceOf[Key]
          }
          val applicablePayloadKey: Option[Key] = modFromCache.collect { case p: Payload if isApplicable(p.id) =>
            logger.info(s"Find new payload for next best header: ${Algos.encode(p.id)}")
            val k: Key = new mutable.WrappedArray.ofByte(p.id)
            k
          }
          applicablePayloadKey
        case None =>
          logger.info(s"No payloads for current history")
          None
      }
      val payloadKey: List[Key] = List(payloadId).collect { case Some(k: Key) => k }
      if (payloadKey.isEmpty) {
        logger.info(s"PayloadKeys is empty. Starting exhaustiveSearch.")
        exhaustiveSearch
      }
      else payloadKey
    } else {
      logger.info(s"Starting exhaustiveSearch.")
      exhaustiveSearch
    }
  }
}