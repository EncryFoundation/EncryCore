package encry.api.http

import java.net.{InetAddress, InetSocketAddress}

import akka.actor.{Actor, ActorRef, Props, Stash}
import com.typesafe.scalalogging.StrictLogging
import encry.EncryApp._
import encry.api.http.DataHolderForApi._
import akka.pattern._
import akka.util.Timeout
import encry.EncryApp
import encry.api.http.routes.InfoApiRoute
import encry.api.http.routes.PeersApiRoute.PeerInfoResponse
import encry.network.NodeViewSynchronizer.ReceivableMessages.{ChangedHistory, ChangedState, NodeViewChange, PeerFromCli, RemovePeerFromBlackList, UpdatedHistory}
import encry.settings.EncryAppSettings
import encry.utils.{NetworkTime, NetworkTimeProvider}
import encry.view.state.{UtxoState, UtxoStateReader}
import encry.local.miner.Miner.{DisableMining, EnableMining, MinerStatus, StartMining}
import encry.network.BlackList.BanReason.InvalidNetworkMessage
import encry.network.BlackList.{BanReason, BanTime, BanType}
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.network.PeersKeeper.{BanPeer, BanPeerFromAPI}
import encry.storage.VersionalStorage.{StorageValue, StorageVersion}
import encry.view.NodeViewHolder.CurrentView
import encry.view.NodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import encry.view.history.History
import encry.view.wallet.EncryWallet
import org.encryfoundation.common.crypto.PrivateKey25519
import org.encryfoundation.common.modifiers.history.{Block, Header}
import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
import scorex.utils.Random

import scala.concurrent.Future

class DataHolderForApi(settings: EncryAppSettings, ntp: NetworkTimeProvider) extends Actor with StrictLogging
with Stash {

  implicit val timeout: Timeout = Timeout(settings.restApi.timeout)

  var permissionedUsers: Seq[String] = Seq("127.0.0.1")

  override def preStart(): Unit =
    context.system.eventStream.subscribe(self, classOf[NodeViewChange])

  override def receive: Receive = awaitNVHRef

  val liorl00Dir: LiorL00Storage = LiorL00Storage.init(settings)

  override def postStop(): Unit = liorl00Dir.storage.close()

  def awaitNVHRef: Receive = {
    case UpdatedHistory(history) =>
      unstashAll()
    context.become(workingCycle(nvhRef = sender(), history = Some(history)))
    case PasswordForLiorL00Storage(pass) =>
      stash()
    case GetNodePass =>
      sender() ! liorl00Dir.getPassword
    case i => println(i)
  }

  def workingCycle(nvhRef: ActorRef,
                    blackList: Seq[(InetAddress, (BanReason, BanTime, BanType))] = Seq.empty,
                   connectedPeers: Seq[ConnectedPeer] = Seq.empty,
                   history: Option[History] = None,
                   state: Option[UtxoStateReader] = None,
                   transactionsOnMinerActor: Int = 0,
                   minerStatus: MinerStatus = MinerStatus(isMining = false, None),
                   blockInfo: BlockAndHeaderInfo = BlockAndHeaderInfo(None, None),
                   allPeers: Seq[InetSocketAddress] = Seq.empty): Receive = {
    case UpdatingTransactionsNumberForApi(qty) =>
      context.become(workingCycle(nvhRef, blackList, connectedPeers, history, state, qty, minerStatus, blockInfo, allPeers))

    case GetNodePass =>
      sender() ! liorl00Dir.getPassword

    case PasswordForLiorL00Storage(pass) =>
      liorl00Dir.putPassword(pass)

    case BlockAndHeaderInfo(header, block) =>
      context.become(
        workingCycle(nvhRef, blackList,
                     connectedPeers,
                     history,
                     state,
                     transactionsOnMinerActor,
                     minerStatus,
                     BlockAndHeaderInfo(header, block),
                     allPeers)
      )

    case ChangedHistory(reader: History) =>
      context.become(
        workingCycle(nvhRef, blackList,
                     connectedPeers,
                     Some(reader),
                     state,
                     transactionsOnMinerActor,
                     minerStatus,
                     blockInfo,
                     allPeers)
      )

    case ChangedState(reader: UtxoStateReader) =>
      context.become(
        workingCycle(nvhRef, blackList,
                     connectedPeers,
                     history,
                     Some(reader),
                     transactionsOnMinerActor,
                     minerStatus,
                     blockInfo,
                     allPeers)
      )

    case UpdatingMinerStatus(status) =>
      context.become(
        workingCycle(nvhRef, blackList, connectedPeers, history, state, transactionsOnMinerActor, status, blockInfo, allPeers)
      )

    case UpdatingPeersInfo(allP, connectedP, bannedP) =>
      context.become(
        workingCycle(nvhRef, bannedP, connectedP, history, state, transactionsOnMinerActor, minerStatus, blockInfo, allP)
      )

    case PeerAdd(peer)               => context.system.eventStream.publish(PeerFromCli(peer))
    case RemovePeerFromBanList(peer) =>
      //nodeViewSynchronizer ! RemovePeerFromBlackList(peer)
      context.system.eventStream.publish(RemovePeerFromBlackList(peer))

    case GetViewPrintAddress =>
      (self ? GetDataFromPresentView[History, UtxoState, EncryWallet, String] { view =>
        view.vault.publicKeys.foldLeft("") { (str, k) =>
          str + s"Pay2PubKeyAddress : ${k.address.address} , Pay2ContractHashAddress : ${k.address.p2ch.address}" + "\n"
        }
      }).pipeTo(sender)

    case GetViewCreateKey =>
      (self ? GetDataFromPresentView[History, UtxoState, EncryWallet, PrivateKey25519] { view =>
        if (view.vault.accountManager.accounts.isEmpty) view.vault.accountManager.mandatoryAccount
        else view.vault.accountManager.createAccount(None)
      }).pipeTo(sender)

    case GetViewPrintPubKeys =>
      (self ? GetDataFromPresentView[History, UtxoState, EncryWallet, List[String]] { view =>
        view.vault.publicKeys.foldLeft(List.empty[String])((str, k) => str :+ Algos.encode(k.pubKeyBytes))
      }).pipeTo(sender)

    case GetViewGetBalance =>
      (self ? GetDataFromPresentView[History, UtxoState, EncryWallet, Map[String, Amount]] { view =>
        val balance: Map[String, Amount] = view.vault.getBalances.toMap
        //        println(balance +" 123124")
        if (balance.isEmpty) Map.empty[String, Amount] else balance
      }).pipeTo(sender)

    case GetViewPrintPrivKeys =>
      (self ? GetDataFromPresentView[History, UtxoState, EncryWallet, String] { view =>
        view.vault.accountManager.accounts.foldLeft("")((str, k) => str + Algos.encode(k.privKeyBytes) + "\n")
      }).pipeTo(sender)

    case GetLastHeadersHelper(i) =>
      (self ? GetDataFromHistory)
        .mapTo[History]
        .map {
          _.lastHeaders(i).headers
        }
        .pipeTo(sender)

    case GetFullHeaderById(id) =>
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

    case GetBannedPeersHelper =>
      (self ? GetBannedPeers)
        .mapTo[Seq[(InetAddress, (BanReason, BanTime, BanType))]]
        .map(_.map(_.toString))
        .pipeTo(sender)

    case GetAllPermissionedUsers =>
      sender() ! permissionedUsers

    case GetBannedPeersHelperAPI =>
      (self ? GetBannedPeers)
        .mapTo[Seq[(InetAddress, (BanReason, BanTime, BanType))]]
        .pipeTo(sender)

    case GetConnectedPeersHelper =>
      (self ? GetConnectedPeers)
        .mapTo[Seq[ConnectedPeer]]
        .map(
          _.map(
            peer =>
              PeerInfoResponse(peer.socketAddress.toString,
                               Some(peer.handshake.nodeName),
                               Some(peer.direction.toString))
          )
        )
        .pipeTo(sender)

    case GetLastHeaderIdAtHeightHelper(i) =>
      (self ? GetDataFromHistory)
        .mapTo[History]
        .map {
          _.headerIdsAtHeight(i).map(Algos.encode)
        }
        .pipeTo(sender)

    case AddUser(ip) => permissionedUsers = permissionedUsers :+ ip

    case GetAllInfoHelper => {

      val getNodeName: String = settings.network.nodeName
        .getOrElse(InetAddress.getLocalHost.getHostAddress + ":" + settings.network.bindAddress.getPort)

      val getStateType: String = "UTXO"

      val storageInfo: String = ""

      val getAddress: Seq[InetSocketAddress] = settings.network.knownPeers

      val getConnectionWithPeers: Boolean = settings.network.connectOnlyWithKnownPeers.getOrElse(false)

      val launchTimeFuture: Future[NetworkTime.Time] = ntp.time()

      val askAllF = (self ? GetAllInfo)
        .mapTo[(Seq[ConnectedPeer], MinerStatus, Readers, Int, BlockAndHeaderInfo, Seq[InetSocketAddress])]
      (for {
        (connectedPeers, minerInfo, stateReader, txsQty, blocksInfo, _) <- askAllF
        currentTime                                                     <- ntp.time()
        launchTime                                                      <- launchTimeFuture
      } yield
        InfoApiRoute.makeInfoJson(
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
        )).pipeTo(sender())
    }

    case GetConnectedPeers     => sender() ! connectedPeers
    case GetDataFromHistory    => history.foreach(sender() ! _)
    case GetMinerStatus        => sender() ! minerStatus
    case GetReaders            => sender() ! Readers(history, state)
    case GetTransactionsNumber => sender() ! transactionsOnMinerActor
    case GetTimeProvider       => sender() ! ntp
    case GetBlockInfo          => sender() ! blockInfo
    case GetAllPeers           => sender() ! allPeers
    case GetBannedPeers        => sender() ! blackList
    case PeerBanHelper(peer, msg)    =>
      context.system.eventStream.publish(BanPeerFromAPI(peer, InvalidNetworkMessage(msg)))
    case StartMiner =>
      context.system.eventStream.publish(EnableMining)
      context.system.eventStream.publish(StartMining)
    case StopMiner                 =>
      context.system.eventStream.publish(DisableMining)
    case ShutdownNode              =>
      EncryApp.forceStopApplication(errorMessage = "Stopped by cli command")
    case GetDataFromPresentView(f) =>
      (nvhRef ? GetDataFromCurrentView(f)).pipeTo(sender)
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

  final case class UpdatingBlackListForApi(blackList: Seq[InetAddress]) extends AnyVal

  final case class UpdatingConnectedListForApi(connectedPeers: Seq[InetSocketAddress]) extends AnyVal

  final case class UpdatingTransactionsNumberForApi(number: Int) extends AnyVal

  final case class UpdatingMinerStatus(minerStatus: MinerStatus) extends AnyVal

  final case class UpdatingPeersInfo(allPeers: Seq[InetSocketAddress],
                                     connectedPeers: Seq[ConnectedPeer],
                                     blackList: Seq[(InetAddress, (BanReason, BanTime, BanType))])

  final case class BlockAndHeaderInfo(header: Option[Header], block: Option[Block])

  final case class RemovePeerFromBanList(peer: InetSocketAddress)

  final case class PeerAdd(peer: InetSocketAddress)

  final case class GetDataFromPresentView[HIS, MS, VL, A](f: CurrentView[HIS, MS, VL] => A)

  final case class GetFullHeaderById(headerId: Either[String, ModifierId])

  final case class GetLastHeadersHelper(i: Int)

  final case class GetLastHeaderIdAtHeightHelper(i: Int)

  final case class Readers(h: Option[History], s: Option[UtxoStateReader])

  final case class AddUser(ip: String)

  final case class PasswordForLiorL00Storage(password: String)

  case class PeerBanHelper(addr: InetSocketAddress, msg: String)

  case object GetViewSendTx

  case object GetNodePass

  case object GetAllPermissionedUsers

  case object GetInfoHelper

  case object GetViewPrintAddress

  case object GetConnectedPeersHelper

  case object GetViewCreateKey

  case object GetViewPrintPubKeys

  case object GetViewGetBalance

  case object GetViewPrintPrivKeys

  case object GetTimeProvider

  case object GetAllInfoHelper

  case object GetBannedPeers

  case object StartMiner

  case object StopMiner

  case object ShutdownNode

  case object GetTransactionsNumber

  case object GetReaders

  case object GetConnectedPeers

  case object GetDataFromHistory

  case object GetMinerStatus

  case object GetAllPeers

  case object GetBlockInfo

  case object GetBannedPeersHelper

  case object GetBannedPeersHelperAPI

  case object GetAllInfo

  def props(settings: EncryAppSettings, ntp: NetworkTimeProvider): Props = Props(new DataHolderForApi(settings, ntp))
}
