package encry.api.http.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import encry.api.http.DataHolderForApi.{GetDataFromHistory, GetFullBlockByIdCommand, GetLastHeaderIdAtHeightHelper, GetLastHeadersHelper, GetMinerStatus}
import encry.local.miner.Miner.MinerStatus
import encry.settings.RESTApiSettings
import encry.view.history.{History, HistoryReader}
import io.circe.syntax._
import org.encryfoundation.common.modifiers.history.{Block, Header}
import org.encryfoundation.common.utils.Algos

case class HistoryApiRoute(dataHolder: ActorRef, settings: RESTApiSettings, nodeId: Array[Byte])(
  implicit val context: ActorRefFactory
) extends EncryBaseApiRoute {

  override val route: Route = pathPrefix("history") {
    getHeaderIdsR ~
      getLastHeadersR ~
      getHeadersIdsAtHeightR ~
      getBlockHeaderByHeaderIdR ~
      getBlockTransactionsByHeaderIdR ~
      getFullBlockByHeaderIdR ~
      candidateBlockR
  }

  def getHeaderIdsR: Route = (pathEndOrSingleSlash & get & paging) { (offset, limit) =>
    (dataHolder ? GetDataFromHistory).mapTo[HistoryReader]
      .map {
      _.getHeaderIds(offset, limit).map(Algos.encode).asJson
    }.okJson()
  }

  def getLastHeadersR: Route = (pathPrefix("lastHeaders" / IntNumber) & get) { qty =>
    (dataHolder ? GetLastHeadersHelper(qty)).mapTo[IndexedSeq[Header]].map(_.asJson).okJson()
  }

  def getHeadersIdsAtHeightR: Route = (pathPrefix("at" / IntNumber) & get) { height =>
    (dataHolder ? GetLastHeaderIdAtHeightHelper(height))
      .mapTo[Seq[String]]
      .map(_.asJson).okJson()
  }

  def getBlockHeaderByHeaderIdR: Route = (modifierId & pathPrefix("header") & get) { id =>
    (dataHolder ? GetFullBlockByIdCommand(Right(id))).mapTo[Option[Block]].map(_.map(x => x.header.asJson)).okJson()
  }

  def getBlockTransactionsByHeaderIdR: Route = (modifierId & pathPrefix("transactions") & get) { id =>
    (dataHolder ? GetFullBlockByIdCommand(Right(id))).mapTo[Option[Block]].map(_.map(_.payload.txs.asJson)).okJson()
  }

  def candidateBlockR: Route = (path("candidateBlock") & pathEndOrSingleSlash & get) {
    (dataHolder ? GetMinerStatus).mapTo[MinerStatus].map(_.json).okJson()
  }

  def getFullBlockByHeaderIdR: Route = (modifierId & get) { id =>
    (dataHolder ? GetFullBlockByIdCommand(Right(id))).mapTo[Option[Block]].map(_.asJson).okJson()
  }
}
