package encry.local.explorer

import akka.actor.Actor
import encry.utils.Logging
import encry.local.explorer.BestChainWriter.{HeaderForDBForks, HeadersList}
import encry.local.explorer.database.DBService
import encry.modifiers.history.Header
import encry.EncryApp.settings

class BestChainWriter(dbService: DBService) extends Actor with Logging {

  override def preStart(): Unit = {
    logInfo(s"Start writing bestChain from this node.")
    context.system.eventStream.subscribe(self, classOf[])
  }

  override def receive: Receive = headersHandler()

  def headersHandler(headersState: List[Header] = List()): Receive = {
    case HeadersList(headers) if headersState.length + 1 > settings.postgres.flatMap(_.writingGap).getOrElse(20) =>
      val headersForDB: List[HeaderForDBForks] =
        headers.map(header => HeaderForDBForks(header.id.toString, header.height))
      dbService.insertHeadersFromNode(headersForDB)
      context.become(headersHandler(List()))

    case HeadersList(headers) =>

  }
}

object BestChainWriter {

  case class HeadersList(headersList: List[Header])

  case class HeaderForDBForks(id: String, height: Int)

}