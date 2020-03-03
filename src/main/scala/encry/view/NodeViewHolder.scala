package encry.view

import java.io.File

import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props}
import akka.dispatch.{PriorityGenerator, UnboundedStablePriorityMailbox}
import akka.pattern._
import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import encry.EncryApp
import encry.EncryApp.{system, timeProvider}
import encry.api.http.DataHolderForApi
import encry.consensus.HistoryConsensus.ProgressInfo
import encry.local.miner.Miner.{DisableMining, StartMining}
import encry.network.DeliveryManager.FullBlockChainIsSynced
import encry.network.Messages.MessageToNetwork.RequestFromLocal
import encry.network.NodeViewSynchronizer.ReceivableMessages._
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.settings.EncryAppSettings
import encry.stats.StatsSender._
import encry.utils.CoreTaggedTypes.VersionTag
import encry.utils.NetworkTimeProvider
import encry.view.NodeViewErrors.ModifierApplyError.HistoryApplyError
import encry.view.NodeViewHolder.ReceivableMessages._
import encry.view.NodeViewHolder._
import encry.view.fast.sync.SnapshotHolder.SnapshotManifest.ManifestId
import encry.view.fast.sync.SnapshotHolder._
import encry.view.history.storage.HistoryStorage
import encry.view.history.{History, HistoryHeadersProcessor, HistoryPayloadsProcessor}
import encry.view.mempool.MemoryPool.RolledBackTransactions
import encry.view.state.UtxoState
import encry.view.state.avlTree.AvlTree
import encry.view.wallet.EncryWallet
import io.iohk.iodb.ByteArrayWrapper
import org.apache.commons.io.FileUtils
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history._
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ADDigest, ModifierId, ModifierTypeId}

import scala.collection.{IndexedSeq, Seq, mutable}
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.util.{Failure, Success, Try}

class NodeViewHolder(memoryPoolRef: ActorRef,
                     influxRef: Option[ActorRef],
                     dataHolder: ActorRef,
                     encrySettings: EncryAppSettings) extends Actor with StrictLogging with AutoCloseable {

  implicit val exCon: ExecutionContextExecutor = context.dispatcher

  context.system.actorSelection("/user/nodeViewSynchronizer") ! ChangedHistory(nodeView.history)

  dataHolder ! UpdatedHistory(nodeView.history)
  dataHolder ! ChangedState(nodeView.state)
  dataHolder ! DataHolderForApi.BlockAndHeaderInfo(nodeView.history.getBestHeader, nodeView.history.getBestBlock)

  influxRef.foreach(ref => context.system.scheduler.schedule(5.second, 5.second) {
    logger.info(s"send info. about ${nodeView.history.getBestHeaderHeight} | ${nodeView.history.getBestBlockHeight} | " +
      s"${nodeView.state.height} -> best header id ${nodeView.history.getBestHeader.map(_.encodedId)} ->" +
      s" best block id ${nodeView.history.getBestBlock.map(_.encodedId)}" +
      s" Best header at best block height ${nodeView.history.getBestBlock.flatMap(b =>
        nodeView.history.getBestHeaderAtHeight(b.header.height)
      ).map(l => l.encodedId -> Algos.encode(l.payloadId))}")
  })

  override def preStart(): Unit = logger.info(s"Node view holder started.")

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    reason.printStackTrace()
    System.exit(100)
  }

  context.system.scheduler.schedule(1.seconds, 10.seconds)(logger.info(s"Modifiers cache from NVH: " +
    s"${ModifiersCache.size}. Elems: ${ModifiersCache.cache.keys.map(key => Algos.encode(key.toArray)).mkString(",")}"))

  override def postStop(): Unit = {
    logger.warn(s"Stopping NodeViewHolder...")
    nodeView.history.closeStorage()
  }

  var potentialManifestIds: List[ManifestId] = List.empty[ManifestId]

  override def receive: Receive = {
    case CreateAccountManagerFromSeed(seed) =>
      val newAccount = nodeView.wallet.addAccount(seed, encrySettings.wallet.map(_.password).get, nodeView.state)
      updateNodeView(updatedVault = newAccount.toOption)
      sender() ! newAccount
    case FastSyncFinished(state, wallet) =>
      logger.info(s"Node view holder got message FastSyncDoneAt. Started state replacing.")
      nodeView.state.tree.avlStorage.close()
      nodeView.wallet.close()
      FileUtils.deleteDirectory(new File(s"${encrySettings.directory}/tmpDirState"))
      FileUtils.deleteDirectory(new File(s"${encrySettings.directory}/keysTmp"))
      FileUtils.deleteDirectory(new File(s"${encrySettings.directory}/walletTmp"))
      logger.info(s"Updated best block in fast sync mod. Updated state height.")
      val newHistory = new History with HistoryHeadersProcessor with HistoryPayloadsProcessor {
        override val settings: EncryAppSettings = encrySettings
        override var isFullChainSynced: Boolean = settings.node.offlineGeneration
        override val timeProvider: NetworkTimeProvider = EncryApp.timeProvider
        override val historyStorage: HistoryStorage = nodeView.history.historyStorage
      }
      newHistory.fastSyncInProgress.fastSyncVal = false
      newHistory.blockDownloadProcessor.updateMinimalBlockHeightVar(nodeView.history.blockDownloadProcessor.minimalBlockHeight)
      newHistory.isHeadersChainSyncedVar = true
      updateNodeView(
        updatedHistory = Some(newHistory),
        updatedState = Some(state),
        updatedVault = Some(wallet)
      )
      system.actorSelection("/user/nodeViewSynchronizer") ! FastSyncDone
      logger.info(s"Fast sync finished successfully!")
    case RemoveRedundantManifestIds => potentialManifestIds = List.empty
    case ModifierFromRemote(mod) =>
      val isInHistory: Boolean = nodeView.history.isModifierDefined(mod.id)
      val isInCache: Boolean = ModifiersCache.contains(key(mod.id))
      if (isInHistory || isInCache)
        logger.info(s"Received modifier of type: ${mod.modifierTypeId}  ${Algos.encode(mod.id)} " +
          s"can't be placed into cache cause of: inCache: ${!isInCache}.")
      else ModifiersCache.put(key(mod.id), mod, nodeView.history)
      computeApplications()

    case lm: LocallyGeneratedModifier =>
      logger.debug(s"Start processing LocallyGeneratedModifier message on NVH.")
      val startTime = System.currentTimeMillis()
      logger.info(s"Got locally generated modifier ${lm.pmod.encodedId} of type ${lm.pmod.modifierTypeId}")
      lm.pmod match {
        case block: Block =>
          pmodModify(block.header, isLocallyGenerated = true)
          pmodModify(block.payload, isLocallyGenerated = true)
        case anyMod =>
          pmodModify(anyMod, isLocallyGenerated = true)
      }
      logger.debug(s"Time processing of msg LocallyGeneratedModifier with mod of type ${lm.pmod.modifierTypeId}:" +
        s" with id: ${Algos.encode(lm.pmod.id)} -> ${System.currentTimeMillis() - startTime}")

    case GetDataFromCurrentView(f) =>
      f(CurrentView(nodeView.history, nodeView.state, nodeView.wallet)) match {
        case resultFuture: Future[_] => resultFuture.pipeTo(sender())
        case result => sender() ! result
      }

    case GetNodeViewChanges(history, state, _) =>
      if (history) sender() ! ChangedHistory(nodeView.history)
      if (state) sender() ! ChangedState(nodeView.state)

    case CompareViews(peer, modifierTypeId, modifierIds) =>
      logger.info(s"Start processing CompareViews message on NVH.")
      val startTime = System.currentTimeMillis()
      val ids: Seq[ModifierId] = modifierTypeId match {
        case _ => modifierIds
          .filterNot(mid => nodeView.history.isModifierDefined(mid) || ModifiersCache.contains(key(mid)))
      }
      if (modifierTypeId != Transaction.modifierTypeId) logger.debug(s"Got compare view message on NVH from ${peer.socketAddress}." +
        s" Type of requesting modifiers is: $modifierTypeId. Requesting ids size are: ${ids.size}." +
        s" Sending RequestFromLocal with ids to $sender." +
        s"\n Requesting ids are: ${ids.map(Algos.encode).mkString(",")}.")
      if (ids.nonEmpty && (modifierTypeId == Header.modifierTypeId || (nodeView.history.isHeadersChainSynced && modifierTypeId == Payload.modifierTypeId)))
        sender() ! RequestFromLocal(peer, modifierTypeId, ids)
      logger.debug(s"Time processing of msg CompareViews from $sender with modTypeId $modifierTypeId: ${System.currentTimeMillis() - startTime}")
    case SemanticallySuccessfulModifier(_) =>
    case msg => logger.error(s"Got strange message on nvh: $msg")
  }

}

object NodeViewHolder {

  final case class DownloadRequest(modifierTypeId: ModifierTypeId,
                                   modifierId: ModifierId,
                                   previousModifier: Option[ModifierId] = None) extends NodeViewHolderEvent

  case class CurrentView[HIS, MS, VL](history: HIS, state: MS, vault: VL)

  case class UpdateInformation(history: History,
                               state: UtxoState,
                               failedMod: Option[PersistentModifier],
                               alternativeProgressInfo: Option[ProgressInfo],
                               suffix: IndexedSeq[PersistentModifier])

  object ReceivableMessages {
    case class CreateAccountManagerFromSeed(seed: String)

    case class GetNodeViewChanges(history: Boolean, state: Boolean, vault: Boolean)

    case class GetDataFromCurrentView[HIS, MS, VL, A](f: CurrentView[HIS, MS, VL] => A)

    case object GetWallet

    case class CompareViews(source: ConnectedPeer, modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId])

    final case class ModifierFromRemote(serializedModifiers: PersistentModifier)

    case class LocallyGeneratedModifier(pmod: PersistentModifier)

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