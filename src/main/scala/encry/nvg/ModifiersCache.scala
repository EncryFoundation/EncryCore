package encry.nvg

import com.typesafe.scalalogging.StrictLogging
import encry.settings.EncryAppSettings
import encry.view.history.HistoryReader
import encry.view.history.ValidationError.{FatalValidationError, NonFatalValidationError}
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{Header, Payload}
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId

import scala.collection.concurrent.TrieMap
import scala.collection.immutable.SortedMap
import scala.collection.mutable

object ModifiersCache extends StrictLogging {

  private type Key = mutable.WrappedArray[Byte]

  val cache: TrieMap[Key, (PersistentModifier, Boolean)] = TrieMap.empty

  private var headersCollection: SortedMap[Int, List[ModifierId]] = SortedMap.empty[Int, List[ModifierId]]

  private var isChainSynced = false

  def setChainSynced(): Unit = isChainSynced = true

  def size: Int = cache.size

  def isEmpty: Boolean = size == 0

  def contains(key: Key): Boolean = cache.contains(key)

  def put(
    key: Key,
    value: PersistentModifier,
    history: HistoryReader,
    settings: EncryAppSettings,
    isLocallyGenerated: Boolean
  ): Unit =
    if (!contains(key)) {
      logger.debug(s"Put ${value.encodedId} of type ${value.modifierTypeId} to cache.")
      cache.put(key, value -> isLocallyGenerated)
      value match {
        case header: Header =>
          val possibleHeadersAtCurrentHeight: List[ModifierId] = headersCollection.getOrElse(header.height, List())
          logger.debug(
            s"possibleHeadersAtCurrentHeight(${header.height}): ${possibleHeadersAtCurrentHeight.map(Algos.encode).mkString(",")}"
          )
          val updatedHeadersAtCurrentHeight: List[ModifierId] = header.id :: possibleHeadersAtCurrentHeight
          logger.debug(
            s"updatedHeadersAtCurrentHeight(${header.height}): ${updatedHeadersAtCurrentHeight.map(Algos.encode).mkString(",")}"
          )
          headersCollection = headersCollection.updated(header.height, updatedHeadersAtCurrentHeight)
        case _ =>
      }

      if (size > settings.node.modifiersCacheSize) cache.find {
        case (_, (modifier, _)) =>
          history.testApplicable(modifier) match {
            case Right(_) | Left(_: NonFatalValidationError) => false
            case _                                           => true
          }
      }.map(mod => remove(mod._1))
    }

  def remove(key: Key): Option[(PersistentModifier, Boolean)] = {
    logger.debug(s"Going to delete ${Algos.encode(key.toArray)}. Cache contains: ${cache.get(key).isDefined}.")
    cache.remove(key)
  }

  def popCandidate(history: HistoryReader, settings: EncryAppSettings): List[(PersistentModifier, Boolean)] =
    synchronized {
      findCandidateKey(history, settings).take(1).flatMap(k => remove(k))
    }

  override def toString: String = cache.keys.map(key => Algos.encode(key.toArray)).mkString(",")

  def findCandidateKey(history: HistoryReader, settings: EncryAppSettings): List[Key] = {

    def isApplicable(key: Key): Boolean =
      cache
        .get(key)
        .exists {
          case (modifier, _) =>
            history.testApplicable(modifier) match {
              case Left(_: FatalValidationError) => remove(key); false
              case Right(_)                      => true
              case Left(_)                       => false
            }
        }

    def getHeadersKeysAtHeight(height: Int): List[Key] =
      headersCollection.get(height) match {
        case Some(headersIds) =>
          headersIds.map(new mutable.WrappedArray.ofByte(_)).collect {
            case headerKey if isApplicable(headerKey) => headerKey
          }
        case None =>
          List.empty[Key]
      }

    def exhaustiveSearch: List[Key] =
      List(cache.find {
        case (k, v) =>
          v._1 match {
            case _: Header if history.getBestHeaderId.exists(_ sameElements v._1.parentId) => true
            case p: Payload =>
              val isApplicableMod: Boolean = isApplicable(k)
              logger.info(s"exhaustiveSearch. Payload. ${p.encodedId}. ${Algos.encode(p.headerId)}. " +
                s"isApplicableMod: $isApplicableMod.")
              isApplicableMod
            case sm =>
              val isApplicableMod: Boolean = isApplicable(k)
              logger.info(s"exhaustiveSearch. Something. ${sm.encodedId}. isApplicableMod: $isApplicableMod.")
              isApplicableMod
          }
      }).collect { case Some(v) => v._1 }

    val bestHeadersIds: List[Key] = {
      headersCollection.get(history.getBestHeaderHeight + 1) match {
        case Some(value) =>
          headersCollection = headersCollection - (history.getBestHeaderHeight + 1)
          logger.debug(s"HeadersCollection size is: ${headersCollection.size}")
          logger.debug(s"Drop height ${history.getBestHeaderHeight + 1} in HeadersCollection")
          val res = value.map(cache.get(_)).collect {
            case Some((v: Header, _))
                if (history.getBestHeaderHeight == settings.constants.PreGenesisHeight &&
                  (v.parentId sameElements Header.GenesisParentId) ||
                  history.getHeaderById(v.parentId).nonEmpty) && isApplicable(new mutable.WrappedArray.ofByte(v.id)) =>
              logger.debug(s"Find new bestHeader in cache: ${Algos.encode(v.id)}")
              new mutable.WrappedArray.ofByte(v.id)
          }
          value.map(id => new mutable.WrappedArray.ofByte(id)).filterNot(res.contains).foreach(cache.remove)
          res
        case None =>
          logger.debug(s"${history.getBestHeader}")
          logger.debug(s"${history.getBestHeaderHeight}")
          logger.debug(s"${headersCollection.get(history.getBestHeaderHeight + 1).map(_.map(Algos.encode))}")
          logger.debug(
            s"No header in cache at height ${history.getBestHeaderHeight + 1}. " +
              s"Trying to find in range [${history.getBestHeaderHeight - settings.constants.MaxRollbackDepth}, ${history.getBestHeaderHeight}]"
          )
          (history.getBestHeaderHeight - settings.constants.MaxRollbackDepth to history.getBestHeaderHeight)
            .flatMap(height => getHeadersKeysAtHeight(height))
            .toList
      }
    }
    if (bestHeadersIds.nonEmpty) bestHeadersIds
    else {
      logger.info(s"Cache. Else condition. history.getBestBlockHeight: ${history.getBestBlockHeight}" +
        s" history.headerIdsAtHeight(history.getBestBlockHeight + 1): ${history.headerIdsAtHeight(history.getBestBlockHeight + 1)} " +
        s" ")
      history.headerIdsAtHeight(history.getBestBlockHeight + 1).headOption match {
        case Some(id) =>
          logger.info(s"Cache. Some. Id: ${Algos.encode(id)}.")
          history.getHeaderById(id) match {
            case Some(header: Header) if isApplicable(new mutable.WrappedArray.ofByte(header.payloadId)) =>
              List(new mutable.WrappedArray.ofByte(header.payloadId))
            case _ if history.isFullChainSynced => exhaustiveSearch
            case _                              => List.empty[Key]
          }
        case None if isChainSynced =>
          logger.info(s"No payloads for current history")
          exhaustiveSearch
        case None =>
          logger.info(s"No payloads for current history")
          List.empty[Key]
      }
    }
  }
}
