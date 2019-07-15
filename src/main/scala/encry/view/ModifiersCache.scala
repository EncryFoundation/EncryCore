package encry.view

import com.typesafe.scalalogging.StrictLogging
import encry.settings.EncryAppSettings
import org.encryfoundation.common.utils.constants.TestNetConstants
import encry.view.history.EncryHistory
import encry.view.history.processors.ValidationError.{FatalValidationError, NonFatalValidationError}
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.Header
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
import encry.view.ModifiersCache._
import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.collection.mutable

final case class ModifiersCache(cache: Map[Key, PersistentModifier],
                                headersCollection: SortedMap[Int, List[ModifierId]],
                                settings: EncryAppSettings) extends StrictLogging {

  val size: Int = cache.size

  val isEmpty: Boolean = size == 0

  def contains(key: Key): Boolean = cache.contains(key)

  def put(modifier: PersistentModifier, history: EncryHistory): ModifiersCache =
    if (!contains(key(modifier.id))) {
      logger.debug(s"Modifier of type ${modifier.modifierTypeId} with id ${modifier.encodedId} putted into cache.")
      val updatedCache: Map[Key, PersistentModifier] = cache.updated(key(modifier.id), modifier)
      val updatedHeadersCollection: SortedMap[Int, List[ModifierId]] = modifier match {
        case header: Header =>
          val possibleHeadersAtCurrentHeight: List[ModifierId] =
            headersCollection.getOrElse(header.height, List.empty[ModifierId])
          logger.debug(s"possibleHeadersAtCurrentHeight(${header.height}):" +
            s" ${possibleHeadersAtCurrentHeight.map(Algos.encode).mkString(",")}")
          val updatedHeadersAtCurrentHeight: List[ModifierId] = header.id :: possibleHeadersAtCurrentHeight
          logger.debug(s"updatedHeadersAtCurrentHeight(${header.height}):" +
            s"${updatedHeadersAtCurrentHeight.map(Algos.encode).mkString(",")}")
          headersCollection.updated(header.height, updatedHeadersAtCurrentHeight)
        case _ =>
          logger.debug(s"Modifiers cache got payload. Don't update headers collection.")
          headersCollection
      }
      val resultedCache: Map[Key, PersistentModifier] =
        if (updatedCache.size > settings.node.modifiersCacheSize) updatedCache.find { case (_, mod) =>
          history.testApplicable(mod) match {
            case Right(_) | Left(_: NonFatalValidationError) => false
            case _ => true
          }
        } match {
          case Some(value) => updatedCache - value._1
          case None => updatedCache
        } else updatedCache
      ModifiersCache(resultedCache, updatedHeadersCollection, settings)
    } else this

  def findCandidateKey(history: EncryHistory): List[Key] = {

    def isApplicable(key: Key): Boolean = cache.get(key).exists(modifier => history.testApplicable(modifier) match {
      case Left(_: FatalValidationError) => remove(key); false
      case Right(_) => true
      case Left(_) => false
    })

    def getHeadersKeysAtHeight(height: Int): List[Key] = headersCollection.get(height) match {
      case Some(headersIds) => headersIds.map(key).collect { case headerKey if isApplicable(headerKey) => headerKey }
      case None =>
        logger.debug(s"Can't find headers at height $height in cache")
        List.empty[Key]
    }

    def findApplicablePayloadAtHeight(height: Int): List[Key] = history
      .headerIdsAtHeight(height)
      .flatMap(history.modifierById)
      .collect { case header: Header if isApplicable(key(header.payloadId)) => key(header.payloadId) }
      .toList

    def exhaustiveSearch: List[Key] = cache
      .find { case (key, value) =>
        value match {
          case _: Header if history.bestHeaderOpt.exists(header => header.id.sameElements(value.parentId)) => true
          case _ =>
            val isApplicableMod: Boolean = isApplicable(key)
            logger.debug(s"Try to apply: ${Algos.encode(key.toArray)} and result is: $isApplicableMod")
            isApplicableMod
        }
      }
      .toList
      .collect { case (k, _) => k }

    @tailrec def applicableBestPayloadChain(atHeight: Int = history.bestBlockHeight,
                                            prevKeys: List[Key] = List.empty[Key]): List[Key] = {
      val payloads: List[Key] = findApplicablePayloadAtHeight(atHeight)
      if (payloads.nonEmpty) applicableBestPayloadChain(atHeight + 1, prevKeys ++ payloads)
      else prevKeys
    }

    val bestHeadersIds: List[Key] = headersCollection
      .get(history.bestHeaderHeight + 1) match {
      case Some(elems) =>
        val updatedHeadersCollection: SortedMap[Int, List[ModifierId]] =
          headersCollection - (history.bestHeaderHeight + 1)
        logger.debug(s"HeadersCollection size is: ${headersCollection.size}")
        logger.debug(s"Drop height ${history.bestHeaderHeight + 1} in HeadersCollection")
        val resultedIds: List[Key] = elems
          .map(cache.get(_))
          .collect { case Some(v: Header) if
          ((v.parentId sameElements history.bestHeaderOpt.map(_.id).getOrElse(Array.emptyByteArray)) ||
            (history.bestHeaderHeight == TestNetConstants.PreGenesisHeight &&
              (v.parentId sameElements Header.GenesisParentId)) ||
            history.modifierById(v.parentId).nonEmpty) && isApplicable(key(v.id)) =>
            logger.debug(s"Find new bestHeader in cache: ${Algos.encode(v.id)}")
            key(v.id)
          }
        val updatedCache: Map[Key, PersistentModifier] =
          cache -- elems.map(id => key(id)).filterNot(resultedIds.contains)
        resultedIds
      case None =>
        logger.debug(s"No header in cache at height ${history.bestHeaderHeight + 1}. " +
          s"Trying to find in range [${history.bestHeaderHeight - TestNetConstants.MaxRollbackDepth}," +
          s" ${history.bestHeaderHeight}]")
        val modifiers: List[Key] =
          (history.bestHeaderHeight - TestNetConstants.MaxRollbackDepth to history.bestHeaderHeight)
            .flatMap(getHeadersKeysAtHeight)
            .toList
        modifiers
    }

    if (bestHeadersIds.nonEmpty) bestHeadersIds
    else history
      .headerIdsAtHeight(history.bestBlockHeight + 1)
      .headOption match {
      case Some(id) => history.modifierById(id) match {
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


  override def toString: String = cache.keys.map(key => Algos.encode(key.toArray)).mkString(",")

}

object ModifiersCache extends StrictLogging {

  type Key = mutable.WrappedArray[Byte]

  private var isChainSynced = false

  def setChainSynced(): Unit = isChainSynced = true

  def key(id: ModifierId): mutable.WrappedArray.ofByte = new mutable.WrappedArray.ofByte(id)

  //val cache: TrieMap[Key, PersistentModifier] = TrieMap[Key, PersistentModifier]()
  //private var headersCollection: SortedMap[Int, List[ModifierId]] = SortedMap[Int, List[ModifierId]]()

//  def remove(key: Key): Option[PersistentModifier] = {
//    logger.debug(s"Going to delete ${Algos.encode(key.toArray)}. Cache contains: ${cache.get(key).isDefined}.")
//    cache.remove(key)
//  }

//  def popCandidate(history: EncryHistory): List[PersistentModifier] = synchronized {
//    findCandidateKey(history).flatMap(k => remove(k))
//  }

  //  override def toString: String = cache.keys.map(key => Algos.encode(key.toArray)).mkString(",")
}