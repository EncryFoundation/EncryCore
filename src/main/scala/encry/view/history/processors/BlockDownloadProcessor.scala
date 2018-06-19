package encry.view.history.processors

import encry.modifiers.history.block.header.EncryBlockHeader
import encry.settings.{Constants, NodeSettings}

case class BlockDownloadProcessor(nodeSettings: NodeSettings) {

  private[history] var minimalBlockHeightVar: Int = 0 //Int.MaxValue

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
    if (!nodeSettings.verifyTransactions) Int.MaxValue
    else if (minimalBlockHeightVar == Int.MaxValue) {
      // just synced with the headers chain - determine first full block to apply
      if (nodeSettings.blocksToKeep < 0) Constants.Chain.GenesisHeight // keep all blocks in history
      else if (!nodeSettings.stateMode.isDigest) Constants.Chain.GenesisHeight
      else Math.max(Constants.Chain.GenesisHeight, header.height - nodeSettings.blocksToKeep + 1) // Start from config.blocksToKeep blocks back
    } else if (nodeSettings.blocksToKeep >= 0) Math.max(header.height - nodeSettings.blocksToKeep + 1, minimalBlockHeightVar)
    else Constants.Chain.GenesisHeight
  }
}
