package encry.view

import akka.actor.Actor
import encry.EncryApp
import scorex.core
import scorex.core._
import scorex.core.consensus.History.ProgressInfo
import scorex.core.consensus.{History, SyncInfo}
import scorex.core.network.ConnectedPeer
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.NodeViewHolderEvent
import scorex.core.serialization.Serializer
import scorex.core.transaction._
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.state.{MinimalState, TransactionValidation}
import scorex.core.transaction.wallet.Vault
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58
import supertagged.@@

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

/**
  * Composite local view of the node
  *
  * Contains instances for History, MinimalState, Vault, MemoryPool.
  * The instances are read-only for external world.
  * Updates of the composite view(the instances are to be performed atomically.
  *
  * @tparam P
  * @tparam TX
  * @tparam PMOD
  */
trait NodeViewHolder[P <: Proposition, TX <: Transaction[P], PMOD <: PersistentNodeViewModifier]
  extends Actor with ScorexLogging {

  import NodeViewHolder.ReceivableMessages._
  import NodeViewHolder._
  import scorex.core.network.NodeViewSynchronizer.ReceivableMessages._

  type SI <: SyncInfo
  type HIS <: History[PMOD, SI, HIS]
  type MS <: MinimalState[PMOD, MS]
  type VL <: Vault[P, TX, PMOD, VL]
  type MP <: MemoryPool[TX, MP]
  type NodeView = (HIS, MS, VL, MP)
  type MapKey = scala.collection.mutable.WrappedArray.ofByte

  var nodeView: NodeView = restoreState().getOrElse(genesisState)
  val modifierSerializers: Map[ModifierTypeId, Serializer[_ <: NodeViewModifier]]
  val networkChunkSize: Int
  val modifiersCache: mutable.Map[MapKey, PMOD] = mutable.Map[MapKey, PMOD]()

  /**
    * Restore a local view during a node startup. If no any stored view found
    * (e.g. if it is a first launch of a node) None is to be returned
    */
  def restoreState(): Option[NodeView]

  /**
    * Hard-coded initial view all the honest nodes in a network are making progress from.
    */
  def genesisState: NodeView

  def history(): HIS = nodeView._1

  def minimalState(): MS = nodeView._2

  def vault(): VL = nodeView._3

  def memoryPool(): MP = nodeView._4

  def key(id: ModifierId): MapKey = new mutable.WrappedArray.ofByte(id)

  def txModify(tx: TX): Unit = {
    //todo: async validation?
    val errorOpt: Option[Throwable] = minimalState() match {
      case txValidator: TransactionValidation[P, TX] =>
        txValidator.validate(tx) match {
          case Success(_) => None
          case Failure(e) => Some(e)
        }
      case _ => None
    }
    errorOpt match {
      case None =>
        memoryPool().put(tx) match {
          case Success(newPool) =>
            log.debug(s"Unconfirmed transaction $tx added to the memory pool")
            val newVault: VL = vault().scanOffchain(tx)
            updateNodeView(updatedVault = Some(newVault), updatedMempool = Some(newPool))
            context.system.eventStream.publish(SuccessfulTransaction[P, TX](tx))
          case Failure(e) => context.system.eventStream.publish(FailedTransaction[P, TX](tx, e))
        }
      case Some(e) => context.system.eventStream.publish(FailedTransaction[P, TX](tx, e))
    }
  }

  /**
    * Update NodeView with new components and notify subscribers of changed components
    *
    * @param updatedHistory
    * @param updatedState
    * @param updatedVault
    * @param updatedMempool
    */
  def updateNodeView(updatedHistory: Option[HIS] = None,
                     updatedState: Option[MS] = None,
                     updatedVault: Option[VL] = None,
                     updatedMempool: Option[MP] = None): Unit = {
    val newNodeView: (HIS, MS, VL, MP) = (updatedHistory.getOrElse(history()),
      updatedState.getOrElse(minimalState()),
      updatedVault.getOrElse(vault()),
      updatedMempool.getOrElse(memoryPool()))
    if (updatedHistory.nonEmpty) context.system.eventStream.publish(ChangedHistory(newNodeView._1.getReader))
    if (updatedState.nonEmpty) context.system.eventStream.publish(ChangedState(newNodeView._2.getReader))
    if (updatedVault.nonEmpty) context.system.eventStream.publish(ChangedVault())
    if (updatedMempool.nonEmpty) context.system.eventStream.publish(ChangedMempool(newNodeView._4.getReader))
    nodeView = newNodeView
  }

  def extractTransactions(mod: PMOD): Seq[TX] = mod match {
    case tcm: TransactionsCarryingPersistentNodeViewModifier[P, TX] => tcm.transactions
    case _ => Seq()
  }

  //todo: this method causes delays in a block processing as it removes transactions from mempool and checks
  //todo: validity of remaining transactions in a synchronous way. Do this job async!
  def updateMemPool(blocksRemoved: Seq[PMOD], blocksApplied: Seq[PMOD], memPool: MP, state: MS): MP = {
    val rolledBackTxs: Seq[TX] = blocksRemoved.flatMap(extractTransactions)
    val appliedTxs: Seq[TX] = blocksApplied.flatMap(extractTransactions)
    memPool.putWithoutCheck(rolledBackTxs).filter { tx =>
      !appliedTxs.exists(t => t.id sameElements tx.id) && {
        state match {
          case v: TransactionValidation[P, TX] => v.validate(tx).isSuccess
          case _ => true
        }
      }
    }
  }

  def requestDownloads(pi: ProgressInfo[PMOD]): Unit =
    pi.toDownload.foreach { case (tid, id) => context.system.eventStream.publish(DownloadRequest(tid, id)) }

  def trimChainSuffix(suffix: IndexedSeq[PMOD], rollbackPoint: ModifierId): IndexedSeq[PMOD] = {
    val idx: Int = suffix.indexWhere(_.id.sameElements(rollbackPoint))
    if (idx == -1) IndexedSeq() else suffix.drop(idx)
  }

  @tailrec
  private def updateState(history: HIS,
                          state: MS,
                          progressInfo: ProgressInfo[PMOD],
                          suffixApplied: IndexedSeq[PMOD]): (HIS, Try[MS], Seq[PMOD]) = {
    case class UpdateInformation(history: HIS,
                                 state: MS,
                                 failedMod: Option[PMOD],
                                 alternativeProgressInfo: Option[ProgressInfo[PMOD]],
                                 suffix: IndexedSeq[PMOD])

    requestDownloads(progressInfo)
    val branchingPointOpt: Option[Array[Byte] @@ core.ModifierId.Tag with core.VersionTag.Tag] = progressInfo.branchPoint.map(VersionTag @@ _)
    val (stateToApplyTry: Try[MS], suffixTrimmed: IndexedSeq[PMOD]) = if (progressInfo.chainSwitchingNeeded) {
      if (!state.version.sameElements(branchingPointOpt))
        state.rollbackTo(branchingPointOpt.get) -> trimChainSuffix(suffixApplied, branchingPointOpt.get)
      else Success(state) -> IndexedSeq()
    } else Success(state) -> suffixApplied

    stateToApplyTry match {
      case Success(stateToApply) =>
        log.info(s"Rollback succeed: BranchPoint(${progressInfo.branchPoint.map(Base58.encode)})")
        context.system.eventStream.publish(RollbackSucceed(branchingPointOpt))
        val u0: UpdateInformation = UpdateInformation(history, stateToApply, None, None, suffixTrimmed)
        val uf: UpdateInformation = progressInfo.toApply.foldLeft(u0) { case (u, modToApply) =>
          if (u.failedMod.isEmpty) u.state.applyModifier(modToApply) match {
            case Success(stateAfterApply) =>
              val newHis: HIS = history.reportModifierIsValid(modToApply)
              context.system.eventStream.publish(SemanticallySuccessfulModifier(modToApply))
              //updateState(newHis, stateAfterApply, newProgressInfo, suffixTrimmed :+ modToApply)
              UpdateInformation(newHis, stateAfterApply, None, None, u.suffix :+ modToApply)
            case Failure(e) =>
              val (newHis: HIS, newProgressInfo: ProgressInfo[PMOD]) = history.reportModifierIsInvalid(modToApply, progressInfo)
              context.system.eventStream.publish(SemanticallyFailedModification(modToApply, e))
              //updateState(newHis, stateToApply, newProgressInfo, suffixTrimmed)
              UpdateInformation(newHis, u.state, Some(modToApply), Some(newProgressInfo), u.suffix)
          }
          else u
        }
        uf.failedMod match {
          case Some(mod) => updateState(uf.history, uf.state, uf.alternativeProgressInfo.get, uf.suffix)
          case None => (uf.history, Success(uf.state), uf.suffix)
        }
      case Failure(e) =>
        log.error("Rollback failed: ", e)
        context.system.eventStream.publish(RollbackFailed(branchingPointOpt))
        EncryApp.forceStopApplication(500)
    }
  }

  override def receive: Receive = {
    case ModifiersFromRemote(remote, modifierTypeId, remoteObjects) =>
      modifierSerializers.get(modifierTypeId) foreach { companion =>
        remoteObjects.flatMap(r => companion.parseBytes(r).toOption).foreach {
          case tx: TX@unchecked if tx.modifierTypeId == Transaction.ModifierTypeId => txModify(tx)
          case pmod: PMOD@unchecked =>
            if (history().contains(pmod) || modifiersCache.contains(key(pmod.id)))
              log.warn(s"Received modifier ${pmod.encodedId} that is already in history")
            else modifiersCache.put(key(pmod.id), pmod)
        }
        log.debug(s"Cache before(${modifiersCache.size}): ${modifiersCache.keySet.map(_.array).map(Base58.encode).mkString(",")}")
        var t: Option[PMOD] = None
        do {
          t = {
            modifiersCache.find { case (_, pmod) =>
              history().applicable(pmod)
            }.map { t =>
              val res = t._2
              modifiersCache.remove(t._1)
              res
            }
          }
          t.foreach(pmodModify)
        } while (t.isDefined)

        log.debug(s"Cache after(${modifiersCache.size}): ${modifiersCache.keySet.map(_.array).map(Base58.encode).mkString(",")}")
      }
    case lt: LocallyGeneratedTransaction[P, TX] => txModify(lt.tx)
    case lm: LocallyGeneratedModifier[PMOD] =>
      log.info(s"Got locally generated modifier ${lm.pmod.encodedId} of type ${lm.pmod.modifierTypeId}")
      pmodModify(lm.pmod)
    case GetDataFromCurrentView(f) => sender() ! f(CurrentView(history(), minimalState(), vault(), memoryPool()))
    case GetNodeViewChanges(history, state, vault, mempool) =>
      if (history) sender() ! ChangedHistory(nodeView._1.getReader)
      if (state) sender() ! ChangedState(nodeView._2.getReader)
      if (vault) sender() ! ChangedVault()
      if (mempool) sender() ! ChangedMempool(nodeView._4.getReader)
    case CompareViews(peer, modifierTypeId, modifierIds) =>
      val ids: Seq[ModifierId] = modifierTypeId match {
        case typeId: ModifierTypeId if typeId == Transaction.ModifierTypeId => memoryPool().notIn(modifierIds)
        case _ => modifierIds.filterNot(mid => history().contains(mid) || modifiersCache.contains(key(mid)))
      }
      sender() ! RequestFromLocal(peer, modifierTypeId, ids)
    case a: Any => log.error("Strange input: " + a)
  }

  //todo: update state in async way?
  def pmodModify(pmod: PMOD): Unit = if (!history().contains(pmod.id)) {
    context.system.eventStream.publish(StartingPersistentModifierApplication(pmod))
    log.info(s"Apply modifier ${pmod.encodedId} of type ${pmod.modifierTypeId} to nodeViewHolder")
    history().append(pmod) match {
      case Success((historyBeforeStUpdate, progressInfo)) =>
        log.debug(s"Going to apply modifications to the state: $progressInfo")
        context.system.eventStream.publish(SyntacticallySuccessfulModifier(pmod))
        context.system.eventStream.publish(NewOpenSurface(historyBeforeStUpdate.openSurfaceIds()))
        if (progressInfo.toApply.nonEmpty) {
          val (newHistory: HIS, newStateTry: Try[MS], blocksApplied: Seq[PMOD]) =
            updateState(historyBeforeStUpdate, minimalState(), progressInfo, IndexedSeq())
          newStateTry match {
            case Success(newMinState) =>
              val newMemPool: MP = updateMemPool(progressInfo.toRemove, blocksApplied, memoryPool(), newMinState)
              //we consider that vault always able to perform a rollback needed
              val newVault: VL = if (progressInfo.chainSwitchingNeeded)
                vault().rollback(VersionTag @@ progressInfo.branchPoint.get).get
              else vault()
              blocksApplied.foreach(newVault.scanPersistent)
              log.info(s"Persistent modifier ${pmod.encodedId} applied successfully")
              updateNodeView(Some(newHistory), Some(newMinState), Some(newVault), Some(newMemPool))
            case Failure(e) =>
              log.warn(s"Can`t apply persistent modifier (id: ${pmod.encodedId}, contents: $pmod) to minimal state", e)
              updateNodeView(updatedHistory = Some(newHistory))
              context.system.eventStream.publish(SemanticallyFailedModification(pmod, e))
          }
        } else {
          requestDownloads(progressInfo)
          updateNodeView(updatedHistory = Some(historyBeforeStUpdate))
        }
      case Failure(e) =>
        log.warn(s"Can`t apply persistent modifier (id: ${pmod.encodedId}, contents: $pmod) to history", e)
        context.system.eventStream.publish(SyntacticallyFailedModification(pmod, e))
    }
  } else log.warn(s"Trying to apply modifier ${pmod.encodedId} that's already in history")
}

object NodeViewHolder {

  object ReceivableMessages {

    // Explicit request of NodeViewChange events of certain types.
    case class GetNodeViewChanges(history: Boolean, state: Boolean, vault: Boolean, mempool: Boolean)

    case class GetDataFromCurrentView[HIS, MS, VL, MP, A](f: CurrentView[HIS, MS, VL, MP] => A)

    // Moved from NodeViewSynchronizer as this was only received here
    case class CompareViews(source: ConnectedPeer, modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId])

    case class ModifiersFromRemote(source: ConnectedPeer, modifierTypeId: ModifierTypeId, remoteObjects: Seq[Array[Byte]])

    case class LocallyGeneratedTransaction[P <: Proposition, TX <: Transaction[P]](tx: TX)

    case class LocallyGeneratedModifier[PMOD <: PersistentNodeViewModifier](pmod: PMOD)

  }

  case class DownloadRequest(modifierTypeId: ModifierTypeId, modifierId: ModifierId) extends NodeViewHolderEvent

  case class CurrentView[HIS, MS, VL, MP](history: HIS, state: MS, vault: VL, pool: MP)

}