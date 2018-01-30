package encry.api.http.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import encry.local.mining.EncryMiner.{MiningStatusRequest, MiningStatusResponse}
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.settings.EncryAppSettings
import encry.view.EncryViewReadersHolder.GetDataFromHistory
import encry.view.history.EncryHistoryReader
import io.circe.Json
import io.circe.syntax._
import scorex.core.ModifierId
import scorex.core.settings.RESTApiSettings
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58

import scala.concurrent.Future

case class HistoryApiRoute(readersHolder: ActorRef, miner: ActorRef, appSettings: EncryAppSettings,
                           nodeId: Array[Byte], digest: Boolean)(implicit val context: ActorRefFactory)
  extends EncryBaseApiRoute with ScorexLogging {

  override val route: Route = pathPrefix("blocks") {
    getBlocksR ~
    getLastHeadersR ~
    getBlockIdsAtHeightR ~
    getBlockHeaderByHeaderIdR ~
    getBlockTransactionsByHeaderIdR ~
    getFullBlockByHeaderIdR ~
    candidateBlockR
  }

  override val settings: RESTApiSettings = appSettings.scorexSettings.restApi

  private def getHistory = (readersHolder ? GetDataFromHistory[EncryHistoryReader](r => r)).mapTo[EncryHistoryReader]

  private def getHeaderIdsAtHeight(h: Int): Future[Json] = getHistory.map { history =>
    history.headerIdsAtHeight(h).map(Base58.encode).asJson
  }

  private def getLastHeaders(n: Int): Future[Json] = getHistory.map { history =>
    history.lastHeaders(n).headers.map(_.json).asJson
  }

  private def getHeaderIds(limit: Int, offset: Int): Future[Json] = getHistory.map { history =>
    history.getHeaderIds(limit, offset).map(Base58.encode).asJson
  }

  private def getFullBlockByHeaderId(headerId: ModifierId): Future[Option[EncryBlock]] = getHistory.map { history =>
    history.typedModifierById[EncryBlockHeader](headerId).flatMap(history.getFullBlock)
  }

  def getBlocksR: Route = (pathEndOrSingleSlash & get & paging) { (offset, limit) =>
    getHeaderIds(limit, offset).okJson()
  }

  def getLastHeadersR: Route = (pathPrefix("lastHeaders" / IntNumber) & get) { count => getLastHeaders(count).okJson() }

  def getBlockIdsAtHeightR: Route = (pathPrefix("at" / IntNumber) & get) { height =>
    getHeaderIdsAtHeight(height).okJson()
  }

  def getBlockHeaderByHeaderIdR: Route = (headerId & pathPrefix("header") & get) { id =>
      getFullBlockByHeaderId(id).map(_.map(_.header.json)).okJson()
  }

  def getBlockTransactionsByHeaderIdR: Route = (headerId & pathPrefix("transactions") & get) { id =>
        getFullBlockByHeaderId(id).map(_.map(_.transactions.map(_.json).asJson)).okJson()
  }

  def candidateBlockR: Route = (path("candidateBlock") & pathEndOrSingleSlash & get) {
    (miner ? MiningStatusRequest).mapTo[MiningStatusResponse].map(_.json).okJson()
  }

  def getFullBlockByHeaderIdR: Route = (headerId & get) { id =>
    getFullBlockByHeaderId(id).map(_.map(_.json)).okJson()
  }
}
