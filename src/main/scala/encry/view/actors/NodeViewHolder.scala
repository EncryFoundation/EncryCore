package encry.view.actors

import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props, Stash}
import akka.dispatch.{PriorityGenerator, UnboundedStablePriorityMailbox}
import akka.pattern._
import akka.util.Timeout
import encry.EncryApp.timeProvider
import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import encry.network.NodeViewSynchronizer.ReceivableMessages._
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.settings.EncryAppSettings
import encry.stats.StatsSender._
import encry.view.ModifiersCache
import encry.view.actors.HistoryApplicator.{GetHistory, InitialStateHistoryWallet}
import encry.view.actors.NodeViewHolder.ReceivableMessages._
import encry.view.actors.NodeViewHolder._
import encry.view.actors.StateApplicator.{GetState, StateForNVH}
import encry.view.actors.WalletApplicator.GetWallet
import encry.view.history.History
import encry.view.mempool.MemoryPool.RolledBackTransactions
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history._
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ModifierId, ModifierTypeId}

import scala.collection.{IndexedSeq, Seq, mutable}
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContextExecutor, Future}

class NodeViewHolder(memoryPoolRef: ActorRef,
                     influxRef: Option[ActorRef],
                     dataHolder: ActorRef,
                     settings: EncryAppSettings) extends Actor with StrictLogging with Stash {

  implicit val exCon: ExecutionContextExecutor = context.dispatcher

  var applicationsSuccessful: Boolean = true

  val walletApplicator: ActorRef = context.system.actorOf(WalletApplicator.props, "walletApplicator1")

  val historyApplicator: ActorRef = context.system.actorOf(
    HistoryApplicator.props(self, walletApplicator, stateApplicator, settings, influxRef, dataHolder, timeProvider)
      .withDispatcher("history-applicator-dispatcher"), "historyApplicator")

  lazy val stateApplicator: ActorRef = context.system.actorOf(
    StateApplicator.props(settings, walletApplicator, dataHolder, self)
      .withDispatcher("state-applicator-dispatcher"), name = "stateApplicator")

  override def receive: Receive = awaitingHistory

  def awaitingHistory: Receive = {
    case InitialStateHistoryWallet(history, w, s) =>
      influxRef.foreach(ref => context.system.scheduler.schedule(5.second, 5.second) {
        ref ! HeightStatistics(
          history.getBestHeaderHeight,
          history.getBestBlockHeight
        )
      })
      unstashAll()
      context.become(mainBehaviour(history, w, s))
    case _ => stash()
  }

  def mainBehaviour(history: History, wallet: EncryWallet, state: UtxoState): Receive = {
    case StateForNVH(stateNew) => context.become(mainBehaviour(history, wallet, stateNew))
    case msg@ModifierFromRemote(_) => historyApplicator ! msg
    case msg@LocallyGeneratedBlock(_) => historyApplicator ! msg
    case GetDataFromCurrentView(f) =>
      val senderRef = sender()
      implicit val timeout: Timeout = 30 seconds
      val newHistory = historyApplicator ? GetHistory
      val newState = stateApplicator ? GetState
      val newWallet = walletApplicator ? GetWallet
      newHistory.foreach(
        his => newState.foreach {
          st => newWallet.foreach {
            wal => f(CurrentView(his, st, wal)) match {
              case result =>
                senderRef ! result
            }
          }
        }
      )
    case GetNodeViewChanges(historyL, stateL, _) =>
      if (historyL) sender() ! ChangedHistory(history)
      if (stateL) sender() ! ChangedState(state)
    case TransactionsForWallet(toRemove) => sendUpdatedInfoToMemoryPool(toRemove)
    case CompareViews(peer, modifierTypeId, modifierIds) =>
      val ids: Seq[ModifierId] = modifierIds
        .filterNot(mid => history.isModifierDefined(mid) || ModifiersCache.contains(key(mid)))
      if (modifierTypeId != Transaction.modifierTypeId) logger.debug(s"Got compare view message on NVH from ${peer.socketAddress}." +
        s" Type of requesting modifiers is: $modifierTypeId. Requesting ids size are: ${ids.size}." +
        s" Sending RequestFromLocal with ids to $sender." +
        s"\n Requesting ids are: ${ids.map(Algos.encode).mkString(",")}.")
      if (ids.nonEmpty && (modifierTypeId == Header.modifierTypeId || (history.isHeadersChainSynced && modifierTypeId == Payload.modifierTypeId)))
        sender() ! RequestFromLocal(peer, modifierTypeId, ids)
    case msg => logger.error(s"Got strange message on nvh: $msg")
  }

  def key(id: ModifierId): mutable.WrappedArray.ofByte = new mutable.WrappedArray.ofByte(id)

  def sendUpdatedInfoToMemoryPool(toRemove: Seq[PersistentModifier]): Unit = {
    val rolledBackTxs: IndexedSeq[Transaction] = toRemove
      .flatMap(extractTransactions)
      .toIndexedSeq
    if (rolledBackTxs.nonEmpty)
      memoryPoolRef ! RolledBackTransactions(rolledBackTxs)
  }

  def extractTransactions(mod: PersistentModifier): Seq[Transaction] = mod match {
    case b: Block => b.payload.txs
    case p: Payload => p.txs
    case _ => Seq.empty[Transaction]
  }
}

object NodeViewHolder {

  final case class TransactionsForWallet(toRemove: Seq[PersistentModifier]) extends AnyVal

  final case class DownloadRequest(modifierTypeId: ModifierTypeId,
                                   modifierId: ModifierId) extends NodeViewHolderEvent

  case class CurrentView[HIS, MS, VL](history: HIS, state: MS, vault: VL)

  object ReceivableMessages {

    case class GetNodeViewChanges(history: Boolean, state: Boolean, vault: Boolean)

    case class GetDataFromCurrentView[HIS, MS, VL, A](f: CurrentView[HIS, MS, VL] => A)

    case class CompareViews(source: ConnectedPeer, modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId])

    final case class ModifierFromRemote(serializedModifiers: PersistentModifier) extends AnyVal

    final case class LocallyGeneratedBlock(block: Block) extends AnyVal

  }

  class NodeViewHolderPriorityQueue(settings: ActorSystem.Settings, config: Config)
    extends UnboundedStablePriorityMailbox(
      PriorityGenerator {
        case CompareViews(_, _, _) => 0
        case PoisonPill => 2
        case otherwise => 1
      })

  def props(memoryPoolRef: ActorRef,
            influxRef: Option[ActorRef],
            dataHolder: ActorRef,
            settings: EncryAppSettings): Props =
    Props(new NodeViewHolder(memoryPoolRef, influxRef, dataHolder, settings))
}