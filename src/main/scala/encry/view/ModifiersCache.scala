package encry.view

import com.typesafe.scalalogging.StrictLogging
import encry.EncryApp.settings
import org.encryfoundation.common.utils.constants.TestNetConstants
import encry.view.history.EncryHistory
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.Header
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
import org.encryfoundation.common.validation.{MalformedModifierError, RecoverableModifierError}

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.util.{Failure, Success}

object ModifiersCache extends StrictLogging {

  private type Key = mutable.WrappedArray[Byte]

  val cache: TrieMap[Key, PersistentModifier] = TrieMap[Key, PersistentModifier]()
  private var headersCollection: SortedMap[Int, List[ModifierId]] = SortedMap[Int, List[ModifierId]]()

  private var isChainSynced = false

  def setChainSynced(): Unit = isChainSynced = true

  def size: Int = cache.size

  def isEmpty: Boolean = size == 0

  def contains(key: Key): Boolean = cache.contains(key)

  def put(key: Key, value: PersistentModifier, history: EncryHistory): Unit = if (!contains(key)) {
    logger.info(s"put ${Algos.encode(key.toArray)} to cache")
    cache.put(key, value)
    value match {
      case header: Header =>
        val possibleHeadersAtCurrentHeight: List[ModifierId] = headersCollection.getOrElse(header.height, List())
        logger.info(s"possibleHeadersAtCurrentHeight(${header.height}): ${possibleHeadersAtCurrentHeight.map(Algos.encode).mkString(",")}")
        val updatedHeadersAtCurrentHeight: List[ModifierId] = header.id :: possibleHeadersAtCurrentHeight
        logger.info(s"updatedHeadersAtCurrentHeight(${header.height}): ${updatedHeadersAtCurrentHeight.map(Algos.encode).mkString(",")}")
        headersCollection = headersCollection.updated(header.height, updatedHeadersAtCurrentHeight)
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

  def remove(key: Key): Option[PersistentModifier] = {
    logger.info(s"Going to delete ${Algos.encode(key.toArray)}. Cache contains: ${cache.get(key).isDefined}.")
    cache.remove(key)
  }

  def popCandidate(history: EncryHistory): List[PersistentModifier] = synchronized {
    findCandidateKey(history).flatMap(k => remove(k))
  }

  override def toString: String = cache.keys.map(key => Algos.encode(key.toArray)).mkString(",")

  def findCandidateKey(history: EncryHistory): List[Key] = {

    def isApplicable(key: Key): Boolean = cache.get(key).exists(modifier => history.testApplicable(modifier) match {
      case Failure(_: RecoverableModifierError) => false
      case Failure(_: MalformedModifierError) =>
        remove(key)
        false
      case Failure(_) => false
      case m => m.isSuccess
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
      history.headerIdsAtHeight(height).flatMap(history.modifierById).collect {
        case header: Header if isApplicable(new mutable.WrappedArray.ofByte(header.payloadId)) =>
          new mutable.WrappedArray.ofByte(header.payloadId)
      }
    }.toList

    def exhaustiveSearch: List[Key] = List(cache.find { case (k, v) =>
      v match {
        //case _: Header if history.bestHeaderOpt.exists(header => header.id sameElements v.parentId) => true
        case _ =>
          val isApplicableMod: Boolean = isApplicable(k)
          //logger.info(s"Try to apply: ${Algos.encode(k.toArray)} and result is: $isApplicableMod")
          isApplicableMod
      }
    }).collect { case Some(v) => v._1 }

    @tailrec
    def applicableBestPayloadChain(atHeight: Int = history.bestBlockHeight, prevKeys: List[Key] = List.empty[Key]): List[Key] = {
      val payloads = findApplicablePayloadAtHeight(atHeight)
      if (payloads.nonEmpty) applicableBestPayloadChain(atHeight + 1, prevKeys ++ payloads)
      else prevKeys
    }

    logger.info(s"history.bestHeaderHeight: ${history.bestHeaderHeight}")
    logger.info(s"history.bestBlockHeight: ${history.bestBlockHeight}")
    val bestHeadersIds: List[Key] = {
      headersCollection.get(history.bestHeaderHeight + 1) match {
        case Some(value) =>
          headersCollection = headersCollection - (history.bestHeaderHeight + 1)
          logger.info(s"HeadersCollection size is: ${headersCollection.size}")
          logger.info(s"Drop height ${history.bestHeaderHeight + 1} in HeadersCollection")
          val res = value.map(cache.get(_)).collect {
            case Some(v: Header)
              if ((v.parentId sameElements history.bestHeaderOpt.map(_.id).getOrElse(Array.emptyByteArray)) ||
                  (history.bestHeaderHeight == TestNetConstants.PreGenesisHeight &&
                    (v.parentId sameElements Header.GenesisParentId)
                    ) || history.modifierById(v.parentId).nonEmpty) && isApplicable(new mutable.WrappedArray.ofByte(v.id)) =>
              logger.info(s"Find new bestHeader in cache: ${Algos.encode(v.id)}")
              new mutable.WrappedArray.ofByte(v.id)
          }
          value.map(id => new mutable.WrappedArray.ofByte(id)).filterNot(res.contains).foreach(cache.remove)
          res
        case None =>
          logger.info(s"No header in cache at height ${history.bestHeaderHeight + 1}. " +
            s"Trying to find in range [${history.bestHeaderHeight - TestNetConstants.MaxRollbackDepth}, ${history.bestHeaderHeight}]")
          (history.bestHeaderHeight - TestNetConstants.MaxRollbackDepth to history.bestHeaderHeight).flatMap(height =>
            getHeadersKeysAtHeight(height)
          ).toList
      }
    }
    if (bestHeadersIds.nonEmpty) bestHeadersIds
    else history.headerIdsAtHeight(history.bestBlockHeight + 1).headOption match {
      case Some(id) => history.modifierById(id) match {
        case Some(header: Header) if isApplicable(new mutable.WrappedArray.ofByte(header.payloadId)) =>
          List(new mutable.WrappedArray.ofByte(header.payloadId))
        case _ if !isChainSynced =>
          logger.info(s"ModsCache no applicable payload at height: ${history.bestBlockHeight + 1}.")
          applicableBestPayloadChain()
        case _ => exhaustiveSearch
      }
      case None if isChainSynced =>
        logger.info(s"No payloads for current history")
        exhaustiveSearch
      case None => logger.info(s"No payloads for current history")
        List.empty[Key]
    }
  }
}