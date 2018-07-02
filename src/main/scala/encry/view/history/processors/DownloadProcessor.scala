package encry.view.history.processors

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.ADProofs
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.settings.{Constants, NodeSettings}
import encry.utils.{NetworkTimeProvider, ScorexLogging}
import encry.view.history.Height
import encry.{ModifierId, ModifierTypeId}

import scala.annotation.tailrec

trait DownloadProcessor extends ScorexLogging {

  protected val nodeSettings: NodeSettings

  protected val timeProvider: NetworkTimeProvider

  protected[history] lazy val blockDownloadProcessor: BlockDownloadProcessor = BlockDownloadProcessor(nodeSettings)

  private var isHeadersChainSyncedVar: Boolean = false

  def bestBlockOpt: Option[EncryBlock]

  def bestBlockIdOpt: Option[ModifierId]

  def typedModifierById[T <: EncryPersistentModifier](id: ModifierId): Option[T]

  def contains(id: ModifierId): Boolean

  def headerIdsAtHeight(height: Int): Seq[ModifierId]

  /**
    * @return true if we decide that our chain is synced with the network.
    */
  def isHeadersChainSynced: Boolean = isHeadersChainSyncedVar

  /**
    * Next number of modifiers we should download to synchronize block chain with headers chain
    */
  def modifiersToDownload(howMany: Int, excluding: Iterable[ModifierId]): Seq[(ModifierTypeId, ModifierId)] = {
    @tailrec
    def continuation(height: Height, acc: Seq[(ModifierTypeId, ModifierId)]): Seq[(ModifierTypeId, ModifierId)] = {
      if (acc.lengthCompare(howMany) >= 0) acc
      else {
        headerIdsAtHeight(height).headOption.flatMap(id => typedModifierById[EncryBlockHeader](id)) match {
          case Some(bestHeaderAtThisHeight) =>
            val toDownload = requiredModifiersForHeader(bestHeaderAtThisHeight)
              .filter(m => !excluding.exists(ex => ex sameElements m._2))
              .filter(m => !contains(m._2))
            continuation(Height @@ (height + 1), acc ++ toDownload)
          case None => acc
        }
      }
    }

    bestBlockOpt match {
      case _ if !isHeadersChainSynced => Seq.empty
      case Some(fb) => continuation(Height @@ (fb.header.height + 1), Seq.empty)
      case None => continuation(Height @@ blockDownloadProcessor.minimalBlockHeightVar, Seq.empty)
    }
  }

  /**
    * Checks, whether it's time to download full chain and return toDownload modifiers
    */
  protected def toDownload(header: EncryBlockHeader): Seq[(ModifierTypeId, ModifierId)] = {

    if (!nodeSettings.verifyTransactions) {
      // Regime that do not download and verify transaction
      Seq.empty
    } else if (header.height >= blockDownloadProcessor.minimalBlockHeight) {
      // Already synced and header is not too far back. Download required modifiers
      requiredModifiersForHeader(header)
    } else if (!isHeadersChainSynced && isNewHeader(header)) {
      // Headers chain is synced after this header. Start downloading full blocks
      log.info(s"Headers chain is synced after header ${header.encodedId} at height ${header.height}")
      println("Sync")
      isHeadersChainSyncedVar = true
      blockDownloadProcessor.updateBestBlock(header)
      Seq.empty
    } else {
      Seq.empty
    }
  }

  private def requiredModifiersForHeader(h: EncryBlockHeader): Seq[(ModifierTypeId, ModifierId)] = {
    if (!nodeSettings.verifyTransactions) {
      Seq.empty
    } else if (nodeSettings.stateMode.isDigest) {
      Seq((EncryBlockPayload.modifierTypeId, h.payloadId), (ADProofs.modifierTypeId, h.adProofsId))
    } else {
      Seq((EncryBlockPayload.modifierTypeId, h.payloadId))
    }
  }

  private def isNewHeader(header: EncryBlockHeader): Boolean =
    timeProvider.time() - header.timestamp < Constants.Chain.DesiredBlockInterval.toMillis * 5
}
