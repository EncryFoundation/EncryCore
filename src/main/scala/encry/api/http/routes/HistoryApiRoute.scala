package encry.api.http.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import encry.ModifierId
import encry.local.miner.EncryMiner.{GetMinerStatus, MinerStatus}
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.settings.{Algos, EncryAppSettings, RESTApiSettings}
import encry.utils.ScorexLogging
import encry.view.EncryViewReadersHolder.GetDataFromHistory
import encry.view.history.EncryHistoryReader
import encry.view.state.StateMode
import io.circe.Json
import io.circe.syntax._

import scala.concurrent.Future

case class HistoryApiRoute(readersHolder: ActorRef, miner: ActorRef, appSettings: EncryAppSettings,
                           nodeId: Array[Byte], stateMode: StateMode)(implicit val context: ActorRefFactory)
  extends EncryBaseApiRoute with ScorexLogging {

  override val route: Route = pathPrefix("history") {
    getBlocksR ~
    getLastHeadersR ~
    getBlockIdsAtHeightR ~
    getBlockHeaderByHeaderIdR ~
    getBlockTransactionsByHeaderIdR ~
    getFullBlockByHeaderIdR ~
    candidateBlockR
  }

  override val settings: RESTApiSettings = appSettings.restApi

  private def getHistory: Future[EncryHistoryReader] = (readersHolder ? GetDataFromHistory[EncryHistoryReader](r => r)).mapTo[EncryHistoryReader]

  private def getHeaderIdsAtHeight(h: Int): Future[Json] = getHistory.map { history =>
    history.headerIdsAtHeight(h).map(Algos.encode).asJson
  }

  private def getLastHeaders(n: Int): Future[Json] = getHistory.map { history =>
    history.lastHeaders(n).headers.map(_.asJson).asJson
  }

  private def getHeaderIds(limit: Int, offset: Int): Future[Json] = getHistory.map { history =>
    history.getHeaderIds(limit, offset).map(Algos.encode).asJson
  }

  private def getFullBlockByHeaderId(headerId: ModifierId): Future[Option[EncryBlock]] = getHistory.map { history =>
    history.typedModifierById[EncryBlockHeader](headerId).flatMap(history.getBlock)
  }

  def getBlocksR: Route = (pathEndOrSingleSlash & get & paging) { (offset, limit) =>
    getHeaderIds(limit, offset).okJson()
  }

  def getLastHeadersR: Route = (pathPrefix("lastHeaders" / IntNumber) & get) { count =>
    getLastHeaders(count).okJson()
  }

  def getBlockIdsAtHeightR: Route = (pathPrefix("at" / IntNumber) & get) { height =>
    getHeaderIdsAtHeight(height).okJson()
  }

  def getBlockHeaderByHeaderIdR: Route = (modifierId & pathPrefix("header") & get) { id =>
    getFullBlockByHeaderId(id).map(_.map(_.header.asJson)).okJson()
  }

  def getBlockTransactionsByHeaderIdR: Route = (modifierId & pathPrefix("transactions") & get) { id =>
    getFullBlockByHeaderId(id).map(_.map(_.transactions.map(_.asJson).asJson)).okJson()
  }

  def candidateBlockR: Route = (path("candidateBlock") & pathEndOrSingleSlash & get) {
    (miner ? GetMinerStatus).mapTo[MinerStatus].map(_.json).okJson()
  }

  def getFullBlockByHeaderIdR: Route = (modifierId & get) { id =>
    getFullBlockByHeaderId(id).map(_.map(_.asJson)).okJson()
  }
}
