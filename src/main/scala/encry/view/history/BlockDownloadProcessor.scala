package encry.view.history

import encry.settings.NodeSettings
import org.encryfoundation.common.utils.constants.TestNetConstants

/**
  * Class that keeps and calculates minimal height for full blocks starting from which we need to download these full
  * blocks from the network and keep them in our history.
  * 'minimalBlockHeight' - Start height to download full blocks. Int.MaxValue when headers chain is not synchronized
  * with the network and no full blocks download needed
  *
  **/
final case class BlockDownloadProcessor(minimalBlockHeight: Int,
                                        isHeadersChainSynced: Boolean,
                                        nodeSettings: NodeSettings) {

  /** Update minimal full block height
    *
    * @param headerHeight - header of new best full block
    * @return minimal height to process best full block */
  def updateBestBlock(headerHeight: Int): (Int, BlockDownloadProcessor) = {
    val newHeight: Int =
      if (minimalBlockHeight == Int.MaxValue) {
        // just synced with the headers chain - determine first full block to apply
        // TODO: start with the height of UTXO snapshot applied. Start from genesis until this is implemented
        if (nodeSettings.blocksToKeep < 0) TestNetConstants.GenesisHeight // keep all blocks in history
        // Start from config.blocksToKeep blocks back
        else Math.max(TestNetConstants.GenesisHeight, headerHeight - nodeSettings.blocksToKeep + 1)
      } else if (nodeSettings.blocksToKeep >= 0) Math.max(headerHeight - nodeSettings.blocksToKeep + 1, minimalBlockHeight)
      else TestNetConstants.GenesisHeight
    (newHeight, BlockDownloadProcessor(newHeight, isHeadersChainSynced, nodeSettings))
  }

  def chainIsSynced: BlockDownloadProcessor =
    BlockDownloadProcessor(minimalBlockHeight, isHeadersChainSynced = true, nodeSettings)
}

object BlockDownloadProcessor {
  def empty(nodeSettings: NodeSettings): BlockDownloadProcessor =
    BlockDownloadProcessor(Int.MaxValue, isHeadersChainSynced = false, nodeSettings)
}