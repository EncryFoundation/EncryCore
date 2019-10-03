package encry.cli.commands.info

import java.net.{InetAddress, InetSocketAddress}

import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor.ActorRef
import encry.api.http.DataHolderForApi.{BlockAndHeaderInfo, GetAllInfo, GetTimeProvider, Readers}
import encry.api.http.routes.InfoApiRoute
import encry.cli.Response
import encry.cli.commands.Command
import encry.local.miner.Miner.MinerStatus
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.settings.EncryAppSettings
import akka.pattern._
import akka.util.Timeout
import encry.utils.NetworkTime.Time
import encry.utils.{NetworkTime, NetworkTimeProvider}
import io.circe.syntax._
import scala.concurrent.duration._

import scala.concurrent.{Await, Future}

object GetInfo extends Command{
  override def execute(args: Command.Args, settings: EncryAppSettings, dataHolder: ActorRef, nodeId: Array[Byte],
                       networkTimeProvider: NetworkTimeProvider): Future[Option[Response]] = {
    implicit val timeout: Timeout = Timeout(settings.restApi.timeout)
     val getNodeName: String = settings.network.nodeName
      .getOrElse(InetAddress.getLocalHost.getHostAddress + ":" + settings.network.bindAddress.getPort)

     val getStateType: String = "UTXO"

     val storageInfo: String = ""

     val getAddress: Seq[InetSocketAddress] = settings.network.knownPeers

     val getConnectionWithPeers: Boolean = settings.network.connectOnlyWithKnownPeers.getOrElse(false)

     val launchTimeFuture: Future[Time] = networkTimeProvider.time()

   val askAllF = (dataHolder ? GetAllInfo)
      .mapTo[(Seq[ConnectedPeer], MinerStatus, Readers, Int, BlockAndHeaderInfo, Seq[InetSocketAddress])]
    (for {
      (connectedPeers, minerInfo, stateReader, txsQty, blocksInfo, _) <- askAllF
      currentTime <- networkTimeProvider.time()
      launchTime  <- launchTimeFuture
    } yield InfoApiRoute.makeInfoJson(
      nodeId,
      minerInfo,
      connectedPeers.size,
      stateReader,
      getStateType,
      getNodeName,
      getAddress,
      storageInfo,
      currentTime - launchTime,
      txsQty,
      getConnectionWithPeers,
      blocksInfo.header,
      blocksInfo.block,
      settings.constants
    )).foreach(x => println(x))
    Future(None)
  }
}
