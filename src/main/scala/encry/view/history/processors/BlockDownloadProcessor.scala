package encry.view.history.processors

import encry.modifiers.history.block.header.EncryBlockHeader
import encry.settings.NodeSettings

case class BlockDownloadProcessor(nodeSettings: NodeSettings){

  private[history] var minimalBlockHeightVar: Int = Int.MaxValue

  /**
    * Start height to download full blocks.
    * Int.MaxValue when headers chain is not synchronized with the network and no full blocks download needed
    */
  def minimalBlockHeight: Int = minimalBlockHeightVar

  /**
    * Update minimal full block height
    *
    * @param header - header of new best full block
    * @return minimal height to process best full block
    */
  def updateBestBlock(header: EncryBlockHeader): Int = {
    minimalBlockHeightVar = minimalBlockHeightAfter(header)
    minimalBlockHeightVar
  }

  private def minimalBlockHeightAfter(header: EncryBlockHeader): Int = {
    if (!nodeSettings.verifyTransactions) {
      // we do not verify transactions at any height
      Int.MaxValue
    } else if (minimalBlockHeightVar == Int.MaxValue) {
      // just synced with the headers chain - determine first full block to apply
      if (nodeSettings.blocksToKeep < 0) {
        // keep all blocks in history
        0
      } else if (!nodeSettings.stateMode.isDigest) {
        ???
      } else {
        // Start from config.blocksToKeep blocks back
        Math.max(0, header.height - nodeSettings.blocksToKeep + 1)
      }
    } else if (nodeSettings.blocksToKeep >= 0) {
      Math.max(header.height - nodeSettings.blocksToKeep + 1, minimalBlockHeightVar)
    } else {
      0
    }
  }
}
