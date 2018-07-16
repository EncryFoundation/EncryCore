package encry.view.history.processors

import encry.modifiers.history.block.header.EncryBlockHeader
import encry.settings.{Constants, NodeSettings}

/** Class that keeps and calculates minimal height for full blocks starting from which we need to download these full
  * blocks from the network and keep them in our history. */
case class BlockDownloadProcessor(nodeSettings: NodeSettings) {

  private[history] var minimalBlockHeightVar: Int = Int.MaxValue

  /** Start height to download full blocks.
    * Int.MaxValue when headers chain is not synchronized with the network and no full blocks download needed */
  def minimalBlockHeight: Int = minimalBlockHeightVar

  /** Update minimal full block height
    *
    * @param header - header of new best full block
    * @return minimal height to process best full block */
  def updateBestBlock(header: EncryBlockHeader): Int = {
    println(s"[DEBUG] [UpdateBestBlock] >> minimalBlockHeightAfter($header) = ${minimalBlockHeightAfter(header)}")
    minimalBlockHeightVar = minimalBlockHeightAfter(header)
    minimalBlockHeightVar
  }

  private def minimalBlockHeightAfter(header: EncryBlockHeader): Int = {
    if (!nodeSettings.verifyTransactions) Int.MaxValue // we do not verify transactions at any height
    else if (minimalBlockHeightVar == Int.MaxValue) {
      // just synced with the headers chain - determine first full block to apply
      if (nodeSettings.blocksToKeep < 0) Constants.Chain.GenesisHeight // keep all blocks in history
      else if (!nodeSettings.stateMode.isDigest) Constants.Chain.GenesisHeight // TODO: start with the height of UTXO snapshot applied. Start from genesis until this is implemented
      else Math.max(Constants.Chain.GenesisHeight, header.height - nodeSettings.blocksToKeep + 1) // Start from config.blocksToKeep blocks back
    } else if (nodeSettings.blocksToKeep >= 0) Math.max(header.height - nodeSettings.blocksToKeep + 1, minimalBlockHeightVar)
    else Constants.Chain.GenesisHeight
  }
}
