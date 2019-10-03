package encry.api.http

import java.net.{InetAddress, InetSocketAddress}

import akka.actor.{Actor, ActorRef, Props}
import com.typesafe.scalalogging.StrictLogging
import encry.EncryApp
import encry.EncryApp._
import akka.pattern._
import encry.api.http.DataHolderForApi._
import akka.pattern._
import akka.util.Timeout
import encry.cli.Response
import encry.network.NodeViewSynchronizer.ReceivableMessages.{ChangedHistory, ChangedState, NodeViewChange, PeerFromCli, RemovePeerFromBlackList}
import encry.settings.EncryAppSettings
import encry.utils.NetworkTimeProvider
import encry.view.state.{UtxoState, UtxoStateReader}
import encry.local.miner.Miner.{DisableMining, EnableMining, MinerStatus, StartMining}
import encry.network.BlackList.{BanReason, BanTime, BanType}
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.view.actors.NodeViewHolder.CurrentView
import encry.view.actors.NodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import encry.view.history.History
import encry.view.wallet.EncryWallet
import org.encryfoundation.common.crypto.PrivateKey25519
import org.encryfoundation.common.modifiers.history.{Block, Header}
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId

import scala.concurrent.Future

class DataHolderForApi(settings: EncryAppSettings,
                       ntp: NetworkTimeProvider) extends Actor with StrictLogging {

  implicit val timeout: Timeout = Timeout(settings.restApi.timeout)

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[NodeViewChange])
  }

  override def receive: Receive = workingCycle()

  def workingCycle(blackList: Seq[(InetAddress, (BanReason, BanTime, BanType))] = Seq.empty,
                   connectedPeers: Seq[ConnectedPeer] = Seq.empty,
                   history: Option[History] = None,
                   state: Option[UtxoStateReader] = None,
                   transactionsOnMinerActor: Int = 0,
                   minerStatus: MinerStatus = MinerStatus(isMining = false, None),
                   blockInfo: BlockAndHeaderInfo = BlockAndHeaderInfo(None, None),
                   allPeers: Seq[InetSocketAddress] = Seq.empty): Receive = {
    case UpdatingTransactionsNumberForApi(qty) =>
      context.become(workingCycle(blackList, connectedPeers, history, state, qty, minerStatus, blockInfo, allPeers))

    case BlockAndHeaderInfo(header, block) =>
      context.become(workingCycle(
      blackList,
      connectedPeers,
      history,
      state,
      transactionsOnMinerActor,
      minerStatus,
      BlockAndHeaderInfo(header, block),
      allPeers))

    case ChangedHistory(reader: History) =>
      context.become(workingCycle(
        blackList,
        connectedPeers,
        Some(reader),
        state,
        transactionsOnMinerActor,
        minerStatus,
        blockInfo,
        allPeers))

    case ChangedState(reader: UtxoStateReader)  =>
      context.become(workingCycle(
        blackList, connectedPeers, history, Some(reader), transactionsOnMinerActor, minerStatus, blockInfo, allPeers)
      )

    case UpdatingMinerStatus(status) =>
      context.become(workingCycle(blackList, connectedPeers, history, state, transactionsOnMinerActor, status, blockInfo, allPeers)
      )

    case UpdatingPeersInfo(allP, connectedP, bannedP) =>
      context.become(workingCycle(
        bannedP, connectedP, history, state, transactionsOnMinerActor, minerStatus, blockInfo, allP)
      )

    case PeerAdd(peer)                => context.system.eventStream.publish(PeerFromCli(peer))
    case RemovePeerFromBanList(peer)  => context.system.eventStream.publish(RemovePeerFromBlackList(peer))

    case GetViewPrintAddress                      =>
      (self ? GetDataFromPresentView[History, UtxoState, EncryWallet, String] { view =>
            view.vault.publicKeys.foldLeft("") { (str, k) =>
              str + s"Pay2PubKeyAddress : ${k.address.address} , Pay2ContractHashAddress : ${k.address.p2ch.address}" + "\n"}
    }).pipeTo(sender)

    case GetViewCreateKey =>
      (self ?
          GetDataFromPresentView[History, UtxoState, EncryWallet, PrivateKey25519] { view =>
            if (view.vault.accountManager.accounts.isEmpty) view.vault.accountManager.mandatoryAccount
            else view.vault.accountManager.createAccount(None)
              }).pipeTo(sender)

    case GetViewPrintPubKeys =>
      (self ?
      GetDataFromPresentView[History, UtxoState, EncryWallet, String] { view =>
        view.vault.publicKeys.foldLeft("")((str, k) => str + Algos.encode(k.pubKeyBytes) + "\n")
      }).pipeTo(sender)

    case GetViewGetBalance =>
      (self ?
      GetDataFromPresentView[History, UtxoState, EncryWallet, String] { view =>
        val balance: String =
          view.vault.getBalances.foldLeft("")((str, tokenInfo) =>
            str.concat(s"TokenID(${tokenInfo._1}) : ${tokenInfo._2}\n"))
        if (balance.length == 0) "0" else balance
      }).pipeTo(sender)

    case GetViewPrintPrivKeys =>
      (self ?
      GetDataFromPresentView[History, UtxoState, EncryWallet, String] { view =>
        view.vault.accountManager.accounts.foldLeft("")((str, k) =>
          str + Algos.encode(k.privKeyBytes) + "\n"
        )
      }).pipeTo(sender)

    case GetLastHeadersHelper(i) =>
      (self ? GetDataFromHistory).mapTo[History].map {
        _.lastHeaders(i).headers
      }.pipeTo(sender)

    case GetFullHeaderById(id) =>
      sender() ! history.flatMap { history =>
         id match {
           case Left(value) =>
             Algos.decode(value).toOption
               .flatMap(decoded => history.getHeaderById(ModifierId @@ decoded))
               .flatMap(history.getBlockByHeader)
           case Right(value) => history.getHeaderById(value).flatMap(history.getBlockByHeader)
         }
       }

    case GetLastHeaderIdAtHeightHelper(i) =>
      (self ? GetDataFromHistory).mapTo[History].map {
        _.headerIdsAtHeight(i).map(Algos.encode)
      }.pipeTo(sender)

    case GetConnectedPeers      => sender() ! connectedPeers
    case GetDataFromHistory     => history.foreach(sender() ! _)
    case GetMinerStatus         => sender() ! minerStatus
    case GetReaders             => sender() ! Readers(history, state)
    case GetTransactionsNumber  => sender() ! transactionsOnMinerActor
    case GetTimeProvider        => sender() ! ntp
    case GetBlockInfo           => sender() ! blockInfo
    case GetAllPeers            => sender() ! allPeers
    case GetBannedPeers         => sender() ! blackList
    case StartMiner             => context.system.eventStream.publish(EnableMining)
                                   context.system.eventStream.publish(StartMining)
    case StopMiner              => context.system.eventStream.publish(DisableMining)
    case ShutdownNode           => EncryApp.forceStopApplication(errorMessage = "Stopped by cli command")
      //
    case GetDataFromPresentView(f) =>
      (nodeViewHolder ? GetDataFromCurrentView(f)).pipeTo(sender)

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

object DataHolderForApi {

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

  case object GetViewPrintAddress

  case object GetViewCreateKey

  case object GetViewPrintPubKeys

  case object GetViewGetBalance

  case object GetViewPrintPrivKeys

  case class GetLastHeadersHelper(i: Int)

  case class GetLastHeaderIdAtHeightHelper(i: Int)

  case object GetTimeProvider

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

  case object GetBannedPeers

  case object GetAllInfo

  final case class Readers(h: Option[History], s: Option[UtxoStateReader])

  def props(settings: EncryAppSettings,
            ntp: NetworkTimeProvider): Props = Props(new DataHolderForApi(settings, ntp))
}