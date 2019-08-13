package encry.view

import com.typesafe.scalalogging.StrictLogging
import encry.EncryApp.settings
import org.encryfoundation.common.utils.constants.TestNetConstants
import encry.view.history.History
import encry.view.history.ValidationError.{FatalValidationError, NonFatalValidationError}
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.Header
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.collection.concurrent.TrieMap
import scala.collection.mutable

object ModifiersCache extends StrictLogging {

  private type Key = mutable.WrappedArray[Byte]

  val cache: TrieMap[Key, PersistentModifier] = TrieMap[Key, PersistentModifier]()
  private var headersCollection: SortedMap[Int, List[ModifierId]] = SortedMap[Int, List[ModifierId]]()

  private var isChainSynced = false

  def setChainSynced(): Unit = isChainSynced = true

  def size: Int = cache.size

  def isEmpty: Boolean = size == 0

  def contains(key: Key): Boolean = cache.contains(key)

  def put(key: Key, value: PersistentModifier, history: History): Unit = if (!contains(key)) {
    logger.debug(s"put ${Algos.encode(key.toArray)} to cache")
    cache.put(key, value)
    value match {
      case header: Header =>
        val possibleHeadersAtCurrentHeight: List[ModifierId] = headersCollection.getOrElse(header.height, List())
        logger.debug(s"possibleHeadersAtCurrentHeight(${header.height}): ${possibleHeadersAtCurrentHeight.map(Algos.encode).mkString(",")}")
        val updatedHeadersAtCurrentHeight: List[ModifierId] = header.id :: possibleHeadersAtCurrentHeight
        logger.debug(s"updatedHeadersAtCurrentHeight(${header.height}): ${updatedHeadersAtCurrentHeight.map(Algos.encode).mkString(",")}")
        headersCollection = headersCollection.updated(header.height, updatedHeadersAtCurrentHeight)
      case _ =>
    }

    if (size > settings.node.modifiersCacheSize) cache.find { case (_, modifier) =>
      history.testApplicable(modifier) match {
        case Right(_) | Left(_: NonFatalValidationError) => false
        case _ => true
      }
    }.map(mod => remove(mod._1))
  }

  def remove(key: Key): Option[PersistentModifier] = {
    logger.debug(s"Going to delete ${Algos.encode(key.toArray)}. Cache contains: ${cache.get(key).isDefined}.")
    cache.remove(key)
  }

  def popCandidate(history: History): List[PersistentModifier] = synchronized {
    findCandidateKey(history).flatMap(k => remove(k))
  }

  override def toString: String = cache.keys.map(key => Algos.encode(key.toArray)).mkString(",")

  def findCandidateKey(history: History): List[Key] = {

    def isApplicable(key: Key): Boolean = cache.get(key).exists(modifier => history.testApplicable(modifier) match {
      case Left(_: FatalValidationError) => remove(key); false
      case Right(_)                      => true
      case Left(_)                       => false
    })

    def getHeadersKeysAtHeight(height: Int): List[Key] = {
      headersCollection.get(height) match {
        case Some(headersIds) =>
          headersIds.map(new mutable.WrappedArray.ofByte(_)).collect {
            case headerKey if isApplicable(headerKey) => headerKey
          }
        case None =>
          logger.debug(s"Can't find headers at height $height in cache")
          List.empty[Key]
      }
    }

    def findApplicablePayloadAtHeight(height: Int): List[Key] = {
      history.headerIdsAtHeight(height).flatMap(history.getHeaderById).collect {
        case header: Header if isApplicable(new mutable.WrappedArray.ofByte(header.payloadId)) =>
          new mutable.WrappedArray.ofByte(header.payloadId)
      }
    }.toList

    def exhaustiveSearch: List[Key] = List(cache.find { case (k, v) =>
      v match {
        case _: Header if history.getBestHeaderId.exists(headerId => headerId sameElements v.parentId) => true
        case _ =>
          val isApplicableMod: Boolean = isApplicable(k)
          logger.debug(s"Try to apply: ${Algos.encode(k.toArray)} and result is: $isApplicableMod")
          isApplicableMod
      }
    }).collect { case Some(v) => v._1 }

    @tailrec
    def applicableBestPayloadChain(atHeight: Int = history.getBestBlockHeight, prevKeys: List[Key] = List.empty[Key]): List[Key] = {
      val payloads = findApplicablePayloadAtHeight(atHeight)
      if (payloads.nonEmpty) applicableBestPayloadChain(atHeight + 1, prevKeys ++ payloads)
      else prevKeys
    }

    val bestHeadersIds: List[Key] = {
      headersCollection.get(history.getBestHeaderHeight + 1) match {
        case Some(value) =>
          headersCollection = headersCollection - (history.getBestHeaderHeight + 1)
          logger.debug(s"HeadersCollection size is: ${headersCollection.size}")
          logger.debug(s"Drop height ${history.getBestHeaderHeight + 1} in HeadersCollection")
          val res = value.map(cache.get(_)).collect {
            case Some(v: Header)
              if ((v.parentId sameElements history.getBestHeaderId.getOrElse(Array.emptyByteArray)) ||
                (history.getBestHeaderHeight == TestNetConstants.PreGenesisHeight &&
                  (v.parentId sameElements Header.GenesisParentId)
                  ) || history.getHeaderById(v.parentId).nonEmpty) && isApplicable(new mutable.WrappedArray.ofByte(v.id)) =>
              logger.debug(s"Find new bestHeader in cache: ${Algos.encode(v.id)}")
              new mutable.WrappedArray.ofByte(v.id)
          }
          value.map(id => new mutable.WrappedArray.ofByte(id)).filterNot(res.contains).foreach(cache.remove)
          res
        case None =>
          logger.debug(s"No header in cache at height ${history.getBestHeaderHeight + 1}. " +
            s"Trying to find in range [${history.getBestHeaderHeight - TestNetConstants.MaxRollbackDepth}, ${history.getBestHeaderHeight}]")
          (history.getBestHeaderHeight - TestNetConstants.MaxRollbackDepth to history.getBestHeaderHeight).flatMap(height =>
            getHeadersKeysAtHeight(height)
          ).toList
      }
    }
    if (bestHeadersIds.nonEmpty) bestHeadersIds
    else history.headerIdsAtHeight(history.getBestBlockHeight + 1).headOption match {
      case Some(id) => history.getHeaderById(id) match {
        case Some(header: Header) if isApplicable(new mutable.WrappedArray.ofByte(header.payloadId)) =>
          List(new mutable.WrappedArray.ofByte(header.payloadId))
        case _ => exhaustiveSearch
      }
      case None if isChainSynced =>
        logger.debug(s"No payloads for current history")
        exhaustiveSearch
      case None => logger.debug(s"No payloads for current history")
        List.empty[Key]
    }
  }
}