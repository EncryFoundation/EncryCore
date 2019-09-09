package encry.view.actors

import org.encryfoundation.common.modifiers.history.{Header, Payload}

import scala.collection.mutable

case class ModifiersCache(headersCache: IndexedSeq[Header],
                          blocksCache: mutable.HashMap[String,Payload]) {

}
