package encry.api.http

import akka.actor.ActorRef

trait HttpAPIDescription {

  /**
    *
    * @return
    */
  def getInfo(p: String): String

  def getBlockById(id: String)
}