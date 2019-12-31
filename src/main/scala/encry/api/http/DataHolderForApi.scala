package encry.api.http

import java.net.{InetAddress, InetSocketAddress}
import akka.actor.{Actor, ActorRef, Props, Stash}
import akka.pattern._
import akka.util.Timeout
import cats.instances.list._
import cats.instances.map._
import cats.syntax.semigroup._
import com.typesafe.scalalogging.StrictLogging
import encry.EncryApp
import encry.EncryApp._
import encry.api.http.DataHolderForApi._
import encry.api.http.routes.InfoApiRoute
import encry.api.http.routes.PeersApiRoute.PeerInfoResponse
import encry.local.miner.Miner.{DisableMining, EnableMining, MinerStatus, StartMining}
import encry.network.BlackList.BanReason.InvalidNetworkMessage
import encry.network.BlackList.{BanReason, BanTime, BanType}
import encry.network.ConnectedPeersCollection
import encry.network.NodeViewSynchronizer.ReceivableMessages._
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.network.PeersKeeper.BanPeerFromAPI
import encry.settings.EncryAppSettings
import encry.utils.{NetworkTime, NetworkTimeProvider}
import encry.view.NodeViewHolder.ReceivableMessages.{CreateAccountManagerFromSeed, GetDataFromCurrentView}
import encry.view.history.History
import encry.view.state.{UtxoState, UtxoStateReader}
import encry.view.wallet.EncryWallet
import org.encryfoundation.common.crypto.PrivateKey25519
import org.encryfoundation.common.modifiers.history.{Block, Header}
import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
import scala.concurrent.Future

class DataHolderForApi(settings: EncryAppSettings, ntp: NetworkTimeProvider)
    extends Actor
    with StrictLogging
    with Stash {

  implicit val timeout: Timeout = Timeout(settings.restApi.timeout)

  override def preStart(): Unit =
    context.system.eventStream.subscribe(self, classOf[NodeViewChange])

  override def receive: Receive = awaitNVHRef

  val storage: AccStorage = AccStorage.init(settings)

  override def postStop(): Unit = storage.storage.close()

  val launchTimeFuture: Future[NetworkTime.Time] = ntp.time()

  def awaitNVHRef: Receive = {
    case UpdatedHistory(history) =>
      unstashAll()
      context.become(workingCycle(nvhRef = sender(), history = Some(history)))
    case PassForStorage(_) =>
      stash()
    case GetNodePassHashAndSalt => sender() ! storage.verifyPassword
  }

  def workingCycle(nvhRef: ActorRef,
                   blackList: Seq[(InetAddress, (BanReason, BanTime, BanType))] = Seq.empty,
                   connectedPeers: Seq[ConnectedPeer] = Seq.empty,
                   history: Option[History] = None,
                   state: Option[UtxoStateReader] = None,
                   transactionsOnMinerActor: Int = 0,
                   minerStatus: MinerStatus = MinerStatus(isMining = false, None),
                   blockInfo: BlockAndHeaderInfo = BlockAndHeaderInfo(None, None),
                   allPeers: Seq[InetSocketAddress] = Seq.empty,
                   connectedPeersCollection: ConnectedPeersCollection = ConnectedPeersCollection()): Receive = {

    case UpdatingTransactionsNumberForApi(qty) =>
      context.become(
        workingCycle(nvhRef,
                     blackList,
                     connectedPeers,
                     history,
                     state,
                     qty,
                     minerStatus,
                     blockInfo,
                     allPeers,
                     connectedPeersCollection)
      )

    case GetNodePassHashAndSalt => sender() ! storage.verifyPassword

    case PassForStorage(pass) => storage.setPassword(pass)

    case ConnectedPeersConnectionHelper(info) =>
      context.become(
        workingCycle(
          nvhRef,
          blackList,
          connectedPeers,
          history,
          state,
          transactionsOnMinerActor,
          minerStatus,
          blockInfo,
          allPeers,
          info
        )
      )

    case blockAndHeader@BlockAndHeaderInfo(_, _) =>
      context.become(
        workingCycle(nvhRef,
                     blackList,
                     connectedPeers,
                     history,
                     state,
                     transactionsOnMinerActor,
                     minerStatus,
                     blockAndHeader,
                     allPeers,
                     connectedPeersCollection)
      )

    case ChangedHistory(reader: History) =>
      context.become(
        workingCycle(nvhRef,
                     blackList,
                     connectedPeers,
                     Some(reader),
                     state,
                     transactionsOnMinerActor,
                     minerStatus,
                     blockInfo,
                     allPeers,
                     connectedPeersCollection)
      )

    case ChangedState(reader: UtxoStateReader) =>
      context.become(
        workingCycle(nvhRef,
                     blackList,
                     connectedPeers,
                     history,
                     Some(reader),
                     transactionsOnMinerActor,
                     minerStatus,
                     blockInfo,
                     allPeers,
                     connectedPeersCollection)
      )

    case UpdatingMinerStatus(status) =>
      context.become(
        workingCycle(nvhRef,
                     blackList,
                     connectedPeers,
                     history,
                     state,
                     transactionsOnMinerActor,
                     status,
                     blockInfo,
                     allPeers,
                     connectedPeersCollection)
      )

    case UpdatingPeersInfo(allP, connectedP, bannedP) =>
      context.become(
        workingCycle(nvhRef,
                     bannedP,
                     connectedP,
                     history,
                     state,
                     transactionsOnMinerActor,
                     minerStatus,
                     blockInfo,
                     allP,
                     connectedPeersCollection)
      )

    case UserAddPeer(peer) => context.system.eventStream.publish(PeerFromCli(peer))

    case RemovePeerFromBanList(peer) => context.system.eventStream.publish(RemovePeerFromBlackList(peer))

    case GetViewPrintAddress =>
      (nvhRef ? GetDataFromCurrentView[History, UtxoState, EncryWallet, String] { view =>
        view.vault.publicKeys.foldLeft("") { (str, k) =>
          str + s"Pay2PubKeyAddress : ${k.address.address} , Pay2ContractHashAddress : ${k.address.p2ch.address}" + "\n"
        }
      }).pipeTo(sender)

    case GetViewCreateKey =>
      (nvhRef ? GetDataFromCurrentView[History, UtxoState, EncryWallet, PrivateKey25519] { view =>
         view.vault.accountManagers.head.createAccount(None)
      }).pipeTo(sender)

    case GetViewPrintPubKeys =>
      (nvhRef ? GetDataFromCurrentView[History, UtxoState, EncryWallet, List[String]] { view =>
        view.vault.publicKeys.foldLeft(List.empty[String])((str, k) => str :+ Algos.encode(k.pubKeyBytes))
      }).pipeTo(sender)

    case GetViewGetBalance =>
      (nvhRef ? GetDataFromCurrentView[History, UtxoState, EncryWallet, Map[String, List[(String, Amount)]]] { view =>
        val balance: Map[String, List[(String, Amount)]] = view.vault.getBalances.map {
          case ((key, token), amount) => Map(key -> List((token, amount)))
        }.foldLeft(Map.empty[String, List[(String, Amount)]]) { case (el1, el2) => el1 |+| el2 }
        if (balance.isEmpty) Map.empty[String, List[(String, Amount)]] else balance
      }).pipeTo(sender)

    case GetViewPrintPrivKeys =>
      (nvhRef ? GetDataFromCurrentView[History, UtxoState, EncryWallet, String] { view =>
        view.vault.accountManagers.head.accounts.foldLeft("")((str, k) => str + Algos.encode(k.privKeyBytes) + "\n")
      }).pipeTo(sender)

   case GetLastHeadersHelper(i) => sender() ! history.toList.flatMap(_.lastHeaders(i).headers)

    case GetFullBlockByIdCommand(id) =>
      sender() ! history.flatMap { history =>
        id match {
          case Left(value) =>
            Algos
              .decode(value)
              .toOption
              .flatMap(decoded => history.getHeaderById(ModifierId @@ decoded))
              .flatMap(history.getBlockByHeader)
          case Right(value) => history.getHeaderById(value).flatMap(history.getBlockByHeader)
        }
      }

    case GetBannedPeersHelper => sender() ! blackList

    case GetConnectedPeersHelper => sender() ! connectedPeers
      .map(
        peer =>
              PeerInfoResponse(peer.socketAddress.toString,
                               Some(peer.handshake.nodeName),
                               Some(peer.direction.toString)))


    case GetLastHeaderIdAtHeightHelper(i) =>
      sender() ! history.toList.flatMap(_.headerIdsAtHeight(i).map(Algos.encode))

    case CreateAccountManagerFromSeedHelper(seed) =>
      (nvhRef ? CreateAccountManagerFromSeed(seed)).mapTo[Either[String, EncryWallet]].pipeTo(sender())

    case GetAllInfoHelper =>

      val getNodeName: String = settings.network.nodeName
        .getOrElse(InetAddress.getLocalHost.getHostAddress + ":" + settings.network.bindAddress.getPort)

      val getStateType: String = "UTXO"

      val storageInfo: String = ""

      val getAddress: Seq[InetSocketAddress] = settings.network.knownPeers

      val getConnectionWithPeers: Boolean = settings.network.connectOnlyWithKnownPeers.getOrElse(false)

      val askAllF = (
        connectedPeers,
        minerStatus,
        Readers(history, state),
        transactionsOnMinerActor,
        blockInfo,
        allPeers)
      (for {
        currentTime  <- ntp.time()
        launchTime   <- launchTimeFuture
      } yield
        InfoApiRoute.makeInfoJson(
          nodeId,
          askAllF._2,
          connectedPeers.size,
          askAllF._3,
          getStateType,
          getNodeName,
          getAddress,
          storageInfo,
          currentTime - launchTime,
          askAllF._4,
          getConnectionWithPeers,
          askAllF._5.header,
          askAllF._5.block,
          settings.constants
        )).pipeTo(sender())

    case GetDataFromHistory    => history.foreach(sender() ! _)
    case GetMinerStatus        => sender() ! minerStatus
    case GetAllPeers           => sender() ! allPeers
    case GetConnections        => sender() ! connectedPeersCollection
    case PeerBanHelper(peer, msg) =>
      context.system.eventStream.publish(BanPeerFromAPI(peer, InvalidNetworkMessage(msg)))
    case StartMinerApiMessage =>
      context.system.eventStream.publish(EnableMining)
      context.system.eventStream.publish(StartMining)
    case StopMinerApiMessage =>
      context.system.eventStream.publish(DisableMining)
    case ShutdownNode =>
      EncryApp.forceStopApplication(errorMessage = "Stopped by cli command")
    case msg@GetDataFromCurrentView(_) => (nvhRef ? msg).pipeTo(sender)
    case GetAllInfo =>
      sender() ! (
        connectedPeers,
        minerStatus,
        Readers(history, state),
        transactionsOnMinerActor,
        blockInfo,
        allPeers
      )
    case _ =>
  }
}

object DataHolderForApi { //scalastyle:ignore

  final case class UpdatingTransactionsNumberForApi(number: Int) extends AnyVal

  final case class UpdatingMinerStatus(minerStatus: MinerStatus) extends AnyVal

  final case class UpdatingPeersInfo(allPeers: Seq[InetSocketAddress],
                                     connectedPeers: Seq[ConnectedPeer],
                                     blackList: Seq[(InetAddress, (BanReason, BanTime, BanType))])

  final case class BlockAndHeaderInfo(header: Option[Header], block: Option[Block])

  final case class RemovePeerFromBanList(peer: InetSocketAddress)

  final case class UserAddPeer(peer: InetSocketAddress)

  final case class GetFullBlockByIdCommand(headerId: Either[String, ModifierId])

  final case class GetLastHeadersHelper(i: Int)

  final case class GetLastHeaderIdAtHeightHelper(i: Int)

  final case class Readers(h: Option[History], s: Option[UtxoStateReader])

  final case class PassForStorage(password: String)

  final case class PeerBanHelper(addr: InetSocketAddress, msg: String)

  final case class ConnectedPeersConnectionHelper(c: ConnectedPeersCollection)

  final case class CreateAccountManagerFromSeedHelper(seed: String)

  case object GetNodePassHashAndSalt

  case object GetViewPrintAddress

  case object GetConnectedPeersHelper

  case object GetViewCreateKey

  case object GetViewPrintPubKeys

  case object GetViewGetBalance

  case object GetViewPrintPrivKeys

  case object GetAllInfoHelper

  case object StartMinerApiMessage

  case object StopMinerApiMessage

  case object ShutdownNode

  case object GetDataFromHistory

  case object GetMinerStatus

  case object GetAllPeers

  case object GetBannedPeersHelper

  case object GetAllInfo

  case object GetConnections

  def props(settings: EncryAppSettings, ntp: NetworkTimeProvider): Props = Props(new DataHolderForApi(settings, ntp))
}
